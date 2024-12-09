package com.softwaremill.quicklens

import scala.quoted.*

object QuicklensMacros {
  def toPathModify[S: Type, A: Type](obj: Expr[S], f: Expr[(A => A) => S])(using Quotes): Expr[PathModify[S, A]] = '{
    PathModify(${ obj }, ${ f })
  }

  def fromPathModify[S: Type, A: Type](pathModify: Expr[PathModify[S, A]])(using Quotes): Expr[(A => A) => S] = '{
    ${ pathModify }.f
  }

  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] = '{ (x: T) => ${ f('x) } }

  def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] = (x: Expr[T]) => '{ $f($x) }

  def modifyLensApplyImpl[T, U](path: Expr[T => U])(using Quotes, Type[T], Type[U]): Expr[PathLazyModify[T, U]] = '{
    PathLazyModify { (t, mod) =>
      ${
        toPathModify('t, modifyImpl('t, Seq(path)))
      }.using(mod)
    }
  }

  def modifyAllLensApplyImpl[T: Type, U: Type](
      path1: Expr[T => U],
      paths: Expr[Seq[T => U]]
  )(using Quotes): Expr[PathLazyModify[T, U]] =
    '{ PathLazyModify((t, mod) => ${ modifyAllImpl('t, path1, paths) }.using(mod)) }

  def modifyAllImpl[S: Type, A: Type](
      obj: Expr[S],
      focus: Expr[S => A],
      focusesExpr: Expr[Seq[S => A]]
  )(using Quotes): Expr[PathModify[S, A]] = {
    val focuses = focusesExpr match {
      case Varargs(args) => focus +: args
    }

    val modF = modifyImpl(obj, focuses)

    toPathModify(obj, modF)
  }

  def toPathModifyFromFocus[S: Type, A: Type](obj: Expr[S], focus: Expr[S => A])(using Quotes): Expr[PathModify[S, A]] =
    toPathModify(obj, modifyImpl(obj, Seq(focus)))

  private def modifyImpl[S: Type, A: Type](
      obj: Expr[S],
      focuses: Seq[Expr[S => A]]
  )(using Quotes): Expr[(A => A) => S] = {
    import quotes.reflect.*

    def unsupportedShapeInfo(tree: Tree) =
      s"Unsupported path element. Path must have shape: _.field1.field2.each.field3.(...), got: ${tree.show}"

    def noSuchMember(tpeStr: String, name: String) =
      s"$tpeStr has no member named $name"

    def noSuitableMember(tpeStr: String, name: String, argNames: Iterable[String]) =
      s"$tpeStr has no member $name with parameters ${argNames.mkString("(", ", ", ")")}"

    def multipleMatchingMethods(tpeStr: String, name: String, syms: Seq[Symbol]) =
      val symsStr = syms.map(s => s" - $s: ${s.termRef.dealias.widen.show}").mkString("\n", "\n", "")
      s"Multiple methods named $name found in $tpeStr: $symsStr"

    def methodSupported(method: String) =
      Seq("at", "each", "eachWhere", "eachRight", "eachLeft", "atOrElse", "index", "when").contains(method)

    enum PathTree:
      case Empty
      case Node(children: Seq[(PathSymbol, Seq[PathTree])])

      def <>(symbols: Seq[PathSymbol]): PathTree = ((this, symbols): @unchecked) match
        case (PathTree.Empty, _) =>
          symbols.toPathTree
        case (PathTree.Node(children), (symbol :: Nil)) =>
          PathTree.Node {
            if children.find(_._1 equiv symbol).isEmpty then children :+ (symbol -> Seq(PathTree.Empty))
            else
              children.map {
                case (sym, trees) if sym equiv symbol =>
                  sym -> (trees :+ PathTree.Empty)
                case c => c
              }
          }
        case (PathTree.Node(children), Nil) =>
          this
        case (PathTree.Node(children), (symbol :: tail)) =>
          PathTree.Node {
            if children.find(_._1 equiv symbol).isEmpty then children :+ (symbol -> Seq(tail.toPathTree))
            else
              children.map {
                case (sym, trees) if sym equiv symbol =>
                  sym -> (trees.init ++ {
                    trees.last match
                      case PathTree.Empty => Seq(PathTree.Empty, tail.toPathTree)
                      case node           => Seq(node <> tail)
                  })
                case c => c
              }
          }
    end PathTree

    object PathTree:
      def empty: PathTree = Empty

    extension (symbols: Seq[PathSymbol])
      def toPathTree: PathTree = symbols match
        case Nil              => PathTree.Empty
        case (symbol :: tail) => PathTree.Node(Seq(symbol -> Seq(tail.toPathTree)))

    enum PathSymbol:
      case Field(override val name: String)
      case Extension(term: Term, override val name: String)
      case FunctionDelegate(override val name: String, givn: Term, typeTree: TypeTree, args: List[Term])
      def name: String

      def equiv(other: Any): Boolean = (this, other) match
        case (Field(name1), Field(name2)) => name1 == name2
        case (Extension(term1, name1), Extension(term2, name2)) => term1 == term2 && name1 == name2
        case (FunctionDelegate(name1, _, typeTree1, args1), FunctionDelegate(name2, _, typeTree2, args2)) =>
          name1 == name2 && typeTree1.tpe == typeTree2.tpe && args1 == args2
        case _ => false
    end PathSymbol

    def toPath(tree: Tree, focus: Expr[S => A]): Seq[PathSymbol] = {
      tree match {
        /** Field access */
        case Select(deep, ident) =>
          toPath(deep, focus) :+ PathSymbol.Field(ident)
        /** Method call with arguments and using clause */
        case Apply(Apply(Apply(TypeApply(Ident(s), typeTrees), idents), args), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath(_, focus)) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, args)
        /** Method call with no arguments and using clause */
        case Apply(Apply(TypeApply(Ident(s), typeTrees), idents), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath(_, focus)) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, List.empty)
        /** Method call with one type parameter and using clause */
        case a @ Apply(TypeApply(Apply(TypeApply(Ident(s), _), idents), typeTrees), List(givn)) if methodSupported(s) =>
          idents.flatMap(toPath(_, focus)) :+ PathSymbol.FunctionDelegate(s, givn, typeTrees.last, List.empty)
        /** Extension method, which is called e.g. as x(_$1) */
        case Apply(obj@Select(term, member), Seq(deep)) if obj.symbol.flags.is(Flags.ExtensionMethod) =>
          toPath(deep, focus) :+ PathSymbol.Extension(term, member)
        /** Field access */
        case Apply(deep, idents) =>
          toPath(deep, focus) ++ idents.flatMap(toPath(_, focus))
        /** Wild card from path */
        case i: Ident if i.name.startsWith("_") =>
          Seq.empty
        case _ =>
          report.errorAndAbort(unsupportedShapeInfo(focus.asTerm))
      }
    }

    extension (tpe: TypeRepr)
      def poorMansLUB: TypeRepr = tpe match {
        case AndType(l, r) if l <:< r => l
        case AndType(l, r) if r <:< l => r
        case _                        => tpe
      }

      def widenAll: TypeRepr =
        tpe.widen.dealias.poorMansLUB

      def matchingTypeSymbol: Symbol = tpe.widenAll match {
        case AndType(l, r) =>
          val lSym = l.matchingTypeSymbol
          if lSym != Symbol.noSymbol then lSym else r.matchingTypeSymbol
        case tpe if isProduct(tpe.typeSymbol) || isSum(tpe.typeSymbol) || isProductLike(tpe.typeSymbol) =>
          tpe.typeSymbol
        case _ =>
          Symbol.noSymbol
      }

    extension (term: Term)
      def appliedToIfNeeded(args: List[Term]): Term =
        if args.isEmpty then term else term.appliedToArgs(args)

    def symbolAccessorByNameOrError(obj: Term, name: String): Term = {
      val objTpe = obj.tpe.widenAll
      val objSymbol = objTpe.matchingTypeSymbol
      // opaque types can find members of underlying types - ignore them (see https://github.com/scala/scala3/issues/22143)
      val fieldMemberSym = objSymbol.fieldMember(name)
      if !objSymbol.flags.is(Flags.Deferred) && fieldMemberSym.exists then
        Select(obj, fieldMemberSym)
      else
        objSymbol.methodMember(name) match
          case List(m) =>
            Select(obj, m)
          case lst =>
            report.errorAndAbort(reportMethodError(objSymbol, name, lst))
    }

    def reportMethodError(sym: Symbol, name: String, lst: List[Symbol], maybeArgNames: Option[Iterable[String]] = None): String = {
      (lst, maybeArgNames) match
        case (Nil, _) => noSuchMember(sym.name, name)
        case (lst, None) => multipleMatchingMethods(sym.name, name, lst)
        case (lst, Some(argNames)) => noSuitableMember(sym.name, name, argNames)
    }

    def methodSymbolByNameOrError(sym: Symbol, name: String): Symbol = {
      sym.methodMember(name) match
        case List(m) => m
        case lst     => report.errorAndAbort(reportMethodError(sym, name, lst))
    }

    def filterMethodsByNameAndArgs(allMethods: List[Symbol], argsMap: Map[String, Term]): Option[Symbol] = {
      val argNames = argsMap.keys
      allMethods.filter { msym =>
        // for copy, we filter out the methods that don't have the desired parameter names
        val paramNames = msym.paramSymss.flatten.filter(_.isTerm).map(_.name)
        argNames.forall(paramNames.contains)
      } match
        case List(m) => Some(m)
        case Nil => None
        case lst@(m :: _) =>
          // if we have multiple matching copy methods, pick the synthetic one, if it exists, otherwise, pick any method
          val syntheticCopies = lst.filter(_.flags.is(Flags.Synthetic))
          syntheticCopies match
            case List(mSynth) => Some(mSynth)
            case _ => Some(m)
    }

    def methodSymbolByNameAndArgs(sym: Symbol, name: String, argsMap: Map[String, Term]): Either[String, Symbol] = {
      if !sym.flags.is(Flags.Deferred) then
        val memberMethods = sym.methodMember(name)
        filterMethodsByNameAndArgs(memberMethods, argsMap)
          .toRight(reportMethodError(sym, name, memberMethods, Some(argsMap.keys)))
      else Left(s"Deferred type ${sym.name}")
    }

    /**
      * @param argsMap normal methods receive one parameter list, extensions methods two, the first one contains the value
      *                on which the extension is called
      * */
    def callMethod(obj: Term, copy: Symbol, argsMap: List[Map[String, Term]]) = {
      require(argsMap.size == 1 || argsMap.size == 2, s"argsMap.size should be either 1 or 2, got: ${argsMap.size} ($argsMap)")
      val objTpe = obj.tpe.widenAll
      val objSymbol = objTpe.matchingTypeSymbol

      val typeParams = objTpe.typeArgs
      val copyTree: DefDef = copy.tree.asInstanceOf[DefDef]
      val copyParams: List[(String, Option[Term])] = copyTree.termParamss.zip(argsMap)
        .map((params, args) => params.params.map(_.name).map(name => name -> args.get(name)))
        .flatten.toList

      val args = copyParams.zipWithIndex.map { case ((n, v), _i) =>
        val i = _i + 1
        def defaultMethod: Term =
          val methodSymbol = methodSymbolByNameOrError(objSymbol, copy.name + "$default$" + i.toString)
          // default values in extension methods take the extension receiver as the first parameter
          val defaultMethodArgs = argsMap.dropRight(1).flatMap(_.values)
          obj.select(methodSymbol).appliedToIfNeeded(defaultMethodArgs)
        n -> v.getOrElse(defaultMethod)
      }.toMap

      val argLists: List[List[Term]] = copyTree.termParamss.take(argsMap.size).map(list => list.params.map(p => args(p.name)))

      if copyTree.termParamss.drop(argLists.size).exists(_.params.exists(!_.symbol.flags.is(Flags.Implicit))) then
        report.errorAndAbort(
          s"Implementation limitation: Only the first parameter list of the modified case classes can be non-implicit. ${copyTree.termParamss.drop(1)}"
        )

      val withTypeParamsApplied = obj.select(copy).appliedToTypes(typeParams)
      argLists.foldLeft(withTypeParamsApplied)(Apply(_, _))
    }

    def termMethodByNameUnsafe(term: Term, name: String): Symbol = {
      val typeSymbol = term.tpe.widenAll.typeSymbol
      methodSymbolByNameOrError(typeSymbol, name)
    }

    def isProduct(sym: Symbol): Boolean = {
      sym.flags.is(Flags.Case)
    }

    def isSum(sym: Symbol): Boolean = {
      (sym.flags.is(Flags.Enum) && !sym.flags.is(Flags.Case)) ||
      (sym.flags.is(Flags.Sealed) && (sym.flags.is(Flags.Trait) || sym.flags.is(Flags.Abstract)))
    }

    def findCompanionLikeObject(objSymbol: Symbol): Symbol = {
      if objSymbol.companionModule.exists then
        objSymbol.companionModule
      else
        val namedFromOwnerScope = objSymbol.owner.fieldMember(objSymbol.name)
        if namedFromOwnerScope.flags.is(Flags.Module) then namedFromOwnerScope
        else Symbol.noSymbol
    }

    def hasExtensionNamed(sym: Symbol, methodName: String): List[Symbol] = {
      val companionSymbol = findCompanionLikeObject(sym)
      if companionSymbol.exists then
        companionSymbol.methodMember(methodName).filter(s => s.name == methodName && s.flags.is(Flags.ExtensionMethod))
      else
        Nil
    }

    def isProductLike(sym: Symbol): Boolean = {
      sym.methodMember("copy").nonEmpty || hasExtensionNamed(sym, "copy").nonEmpty
    }

    def caseClassCopy(
        owner: Symbol,
        mod: Expr[A => A],
        obj: Term,
        fields: Seq[(PathSymbol.Field | PathSymbol.Extension, Seq[PathTree])]
    ): Term = {
      val objTpe = obj.tpe.widenAll
      val objSymbol = objTpe.matchingTypeSymbol
      if isSum(objSymbol) then {
        objTpe match {
          case AndType(_, _) =>
            report.errorAndAbort(
              s"Implementation limitation: Cannot modify sealed hierarchies mixed with & types. Try providing a more specific type."
            )
          case _ =>
        }
        /*
        if (obj.isInstanceOf[Child1]) caseClassCopy(obj.asInstanceOf[Child1]) else
        if (obj.isInstanceOf[Child2]) caseClassCopy(obj.asInstanceOf[Child2]) else
        ...
        else throw new IllegalStateException()
         */
        val ifThens = objSymbol.children.map { child =>
          val ifCond = TypeApply(Select.unique(obj, "isInstanceOf"), List(TypeIdent(child)))

          val ifThen = ValDef.let(owner, TypeApply(Select.unique(obj, "asInstanceOf"), List(TypeIdent(child)))) {
            castToChildVal =>
              caseClassCopy(owner, mod, castToChildVal, fields)
          }

          ifCond -> ifThen
        }

        val elseThrow = '{ throw new IllegalStateException() }.asTerm

        ifThens.foldRight(elseThrow) { case ((ifCond, ifThen), ifElse) =>
          If(ifCond, ifThen, ifElse)
        }
      } else if isProduct(objSymbol) || isProductLike(objSymbol) then {
        val argsMap: Map[String, Term] = fields.map { (field, trees) =>
          val fieldMethod = field match {
            case PathSymbol.Field(name) =>
              symbolAccessorByNameOrError(obj, name)
            case PathSymbol.Extension(term, name) =>
              val extensionMethod = symbolAccessorByNameOrError(term, name)
              Apply(extensionMethod, List(obj))
          }
          val resTerm: Term = trees.foldLeft[Term](fieldMethod) { (term, tree) =>
            mapToCopy(owner, mod, term, tree)
          }
          val namedArg = NamedArg(field.name, resTerm)
          field.name -> namedArg
        }.toMap
        methodSymbolByNameAndArgs(objSymbol, "copy", argsMap) match
          case Right(copy) =>
            callMethod(obj, copy, List(argsMap))
          case Left(error) =>
            val objCompanion = findCompanionLikeObject(objSymbol)
            methodSymbolByNameAndArgs(objCompanion, "copy", argsMap).toOption match
              case Some(copy) =>
                // now try to call the extension as a method, assume the object is its first parameter
                val extensionParameter = copy.paramSymss.headOption.map(_.headOption).flatten
                val argsWithObj = List(extensionParameter.map(name => name.name -> obj).toMap, argsMap)
                callMethod(Ref(objCompanion), copy, argsWithObj)
              case None => report.errorAndAbort(error)
      } else
        report.errorAndAbort(s"Unsupported source object: must be a case class, sealed trait or class with copy method, but got: $objSymbol of type ${objTpe.show} (${obj.show})")
    }

    def applyFunctionDelegate(
        owner: Symbol,
        mod: Expr[A => A],
        objTerm: Term,
        f: PathSymbol.FunctionDelegate,
        tree: PathTree
    ): Term =
      val defdefSymbol = Symbol.newMethod(
        owner,
        "$anonfun",
        MethodType(List("x"))(_ => List(f.typeTree.tpe), _ => f.typeTree.tpe)
      )
      val fMethod = termMethodByNameUnsafe(f.givn, f.name)
      val fun = TypeApply(
        Select(f.givn, fMethod),
        List(f.typeTree)
      )
      val defdefStatements = DefDef(
        defdefSymbol,
        ((_: @unchecked) match
          case List(List(x)) => Some(mapToCopy(defdefSymbol, mod, x.asExpr.asTerm, tree))
        )
      )
      val closure = Closure(Ref(defdefSymbol), None)
      val block = Block(List(defdefStatements), closure)
      Apply(fun, List(objTerm, block) ++ f.args.map(_.changeOwner(owner)))

    def accumulateToCopy(
        owner: Symbol,
        mod: Expr[A => A],
        objTerm: Term,
        pathSymbols: Seq[(PathSymbol, Seq[PathTree])]
    ): Term = pathSymbols match {

      case Nil =>
        objTerm

      case (_: (PathSymbol.Field | PathSymbol.Extension), _) :: _ =>
        val (fs, funs) = pathSymbols.span((ps, _) => ps.isInstanceOf[PathSymbol.Field] || ps.isInstanceOf[PathSymbol.Extension])
        val fields = fs.collect { case (p: (PathSymbol.Field | PathSymbol.Extension), trees) => p -> trees }
        val withCopiedFields: Term = caseClassCopy(owner, mod, objTerm, fields)
        accumulateToCopy(owner, mod, withCopiedFields, funs)

      /** For FunctionDelegate(method, givn, T, args)
        *
        * Generates: `givn.method[T](obj, x => mapToCopy(...), ...args)`
        */
      case (f: PathSymbol.FunctionDelegate, actions: Seq[PathTree]) :: tail =>
        val term = actions.foldLeft(objTerm) { (term, tree) =>
          applyFunctionDelegate(owner, mod, term, f, tree)
        }
        accumulateToCopy(owner, mod, term, tail)
    }

    def mapToCopy(owner: Symbol, mod: Expr[A => A], objTerm: Term, pathTree: PathTree): Term = pathTree match {
      case PathTree.Empty =>
        val apply = termMethodByNameUnsafe(mod.asTerm, "apply")
        // TODO: calling extension may be necessary here
        Apply(Select(mod.asTerm, apply), List(objTerm))
      case PathTree.Node(children) =>
        accumulateToCopy(owner, mod, objTerm, children)
    }

    def extractFocus(tree: Tree): Tree = tree match {
      /** Single inlined path */
      case Inlined(_, _, p) =>
        extractFocus(p)
      /** One of paths from modifyAll */
      case Block(List(DefDef(_, _, _, Some(p))), _) =>
        p
      case _ =>
        report.errorAndAbort(unsupportedShapeInfo(tree))
    }

    val focusesTrees: Seq[Tree] = focuses.map(_.asTerm)
    val paths: Seq[Seq[PathSymbol]] =
      focusesTrees.zip(focuses).map { (tree, focus) =>
        toPath(extractFocus(tree), focus)
      }

    val pathTree: PathTree =
      paths.foldLeft(PathTree.empty) { (tree, path) => tree <> path }

    val res: (Expr[A => A] => Expr[S]) = (mod: Expr[A => A]) =>
      Typed(mapToCopy(Symbol.spliceOwner, mod, obj.asTerm, pathTree), TypeTree.of[S]).asExpr.asInstanceOf[Expr[S]]

    to(res)
  }
}
