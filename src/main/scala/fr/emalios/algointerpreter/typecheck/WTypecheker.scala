package fr.emalios.algointerpreter.typecheck

import fr.emalios.algointerpreter.Main.{debugMode, devDebugMode}
import fr.emalios.algointerpreter.eval.{AlgoTypeCheckingError, In, InOut}
import fr.emalios.algointerpreter.parser
import fr.emalios.algointerpreter.parser.{AlgoAST, Assignment, BinaryOperation, Block, BooleanLiteral, ExprInstr, Expression, ForInstruction, FunctionCall, Identifier, IfThenElseInstruction, Literal, Program, Return, ReturnType, StringLiteral, TypeParameter, TypedExpression, UnaryOperation, WhileInstruction}
import fr.emalios.algointerpreter.token.{And, Equals, Greater, GreaterEqual, Less, LesserEqual, Minus, Mod, Mul, Not, NotEquals, Or, Percent, Plus, Slash, UnaryOperator}
import fr.emalios.algointerpreter.typecheck.algow.{AnyType, BooleanType, CharType, FunctionType, IntegerType, RealType, StringType, TVar, Type, Undefined, UnitType}

import scala.collection.mutable

class WTypecheker {
  def printTypeEnv(newTypeEnv: TypeEnv): Unit = {
    println("Début de l'affichage")
    newTypeEnv.foreach({ case (str, scheme) =>
      println(s"Id: $str a le type ${scheme._2.showType()}")
    })
    println("Fin de l'affichage")
  }

  type Scheme = (List[String], Type)

  type Subst = Map[String, Type]
  val nullSubst: Subst = Map.empty[String, Type]
  def composeSubst(s1: Subst, s2: Subst): Subst = s1.map(subst => (subst._1, this.apply(subst._2)(s1))) ++ s2

  type TypeEnv = mutable.Map[String, Scheme]

  def remove(typeEnv: TypeEnv, str: String): Unit = typeEnv.remove(str)

  /**
   * ftv function return a set of free type variable in a given type
   */
  def ftv(typeOf: Type): Set[String] = {
    typeOf match {
      case TVar(name) => Set(name)
      case FunctionType(parametersType, returnType) =>
        var paramFtv: Set[String] = Set.empty
        parametersType.foreach(param => paramFtv = paramFtv ++ this.ftv(param.paramType))
        val returnFtv: Set[String] = this.ftv(returnType)
        paramFtv ++ returnFtv
      case _ => Set.empty
    }
  }
  def ftv(scheme: Scheme): Set[String] = this.ftv(scheme._2).diff(scheme._1.toSet)
  def ftv(types: List[Type]): Set[String] = types.foldRight(Set.empty[String])((t, seq) => seq.union(this.ftv(t)))
  def ftv(typeEnv: TypeEnv): Set[String] = typeEnv.values.toSet.foldRight(Set.empty[String])((t, seq) => seq.union(this.ftv(t)))

  def apply(typeOf: Type)(subst: Subst): Type = {
    typeOf match {
      case tVar: TVar => subst.get(tVar.name) match {
        case Some(value) => value
        case None => tVar
      }
      case FunctionType(parametersType, returnType) =>
        val paramSubs: Seq[TypeParameter] = Seq.empty
        parametersType.foreach(paramType => paramSubs.+:(TypeParameter(paramType.name, this.apply(paramType.paramType)(subst), paramType.quantifier)))
        val returnSubs: Type = this.apply(returnType)(subst)
        FunctionType(paramSubs, returnSubs)
      case _ => typeOf
    }
  }

  def apply(scheme: Scheme)(subst: Subst): Scheme = {
    val schemeToReturn = (scheme._1, this.apply(scheme._2)(subst))
    schemeToReturn
  }

  def apply(types: List[Type])(subst: Subst): List[Type] = types.map[Type](t => this.apply(t)(subst))
  def apply(typeEnv: TypeEnv)(subst: Subst): TypeEnv = {
    val toReturn = typeEnv.map({case (str, scheme) => (str, this.apply(scheme)(subst))})
    toReturn
  }

  def generalize(typeEnv: TypeEnv, t: Type): Scheme = (this.ftv(t).diff(this.ftv(typeEnv)).toList, t)

  private var i: Int = -1
  def newTVar(char: Char): TVar = {
    i=i+1
    TVar(s"$char$i")
  }

  /**
   * instantiate function replaces all bound type variable in the type scheme with fresh type variable.
   * So if in input we have (List("t0") -> TVar("t0")) in output we have Tvar("t1")
   */
  def instantiate(scheme: Scheme): Type = {
    val nvars: List[TVar] = scheme._1.map(_ => newTVar('t'))
    val map: Map[String, TVar] = Map.from(scheme._1 zip nvars)
    this.apply(scheme._2)(map)
  }

  def mgu(ts1: List[Type], ts2: List[Type]): Subst = {
    var subst: Subst = Map.empty
    (ts1 zip ts2).foreach({case (t1, t2) => subst = this.composeSubst(subst, this.mgu(t1, t2))})
    subst
  }

  /**
   * This is the unification function for types. For two types t1 and t2, mgu(t1, t2) returns the most
   * general unifier. A unifier is a substitution S such that S(t1) ≡ S(t2).
   */
  def mgu(t1: Type, t2: Type): Subst = {
    (t1, t2) match {
      case (FunctionType(params, r), FunctionType(params_, r_)) =>
        val l = params.map(param => param.paramType)
        val l_ = params_.map(param => param.paramType)
        val s1 = this.mgu(l.toList, l_.toList)
        val s2 = (r, r_) match {
          case (t1, t2) => this.mgu(t1, t2)
          case (UnitType, UnitType) => this.nullSubst
          case _ => throw AlgoTypeCheckingError(s"Type mismatch $r and $r_")
        }
        this.composeSubst(s1, s2)
      case (Undefined, _) => this.varBind(this.newTVar('t').name, t2)
      case (_, Undefined) => this.varBind(this.newTVar('t').name, t1)
      case (TVar(name), _) => this.varBind(name, t2)
      case (_, TVar(name)) => this.varBind(name, t1)
      case (IntegerType, IntegerType) => this.nullSubst
      case (BooleanType, BooleanType) => this.nullSubst
      case (_, _) => throw AlgoTypeCheckingError(s"Error: types do not unify: ${t1.showType()} vs. ${t2.showType()}")
    }
  }

  /**
   * The function varBind
   * attempts to bind a type variable to a type and return that binding as a subsitution, but avoids
   * binding a variable to itself and performs the occurs check, which is responsible for circularity
   * type errors.
   */
  def varBind(u: String, t: Type): Subst = {
    if(t.equals(TVar(u))) return this.nullSubst
    if(this.ftv(t).contains(u)) throw AlgoTypeCheckingError("occurs check fails: " + u + " vs. " + t.showType())
    Map((u, t))
  }

  def tiLit(lit: Literal): (Subst, Type) = {
    if(debugMode) println(s"tiLit with lit $lit")
    lit match {
      case StringLiteral(_)  => (this.nullSubst, StringType)
      case parser.Number(_)  => (this.nullSubst, IntegerType)
      case BooleanLiteral(_) => (this.nullSubst, BooleanType)
    }
  }

  /**
   * The function ti infers the types for expressions. The type environment must contain bindings
   * for all free variables of the expressions. The returned substitution records the type constraints
   * imposed on type variables by the expression, and the returned type is the type of the expression.
   */
  def ti(typeEnv: TypeEnv, expr: Expression): (Subst, Type) = {
    expr match {
      case literal: Literal => this.tiLit(literal)
      case BinaryOperation(leftExpression, operator, rightExpression) =>
        val (lSubst, lType) = this.ti(typeEnv, leftExpression)
        val (rSubst, rType) = this.ti(typeEnv, rightExpression)
        val (rExpectedType, lExpectedType, exprType) = operator match {
          case operator: UnaryOperator => operator match {
            case Minus => (IntegerType, IntegerType, IntegerType)
          }
          case Plus => (IntegerType, IntegerType, IntegerType)
          case GreaterEqual => (IntegerType, IntegerType, BooleanType)
          case LesserEqual => (IntegerType, IntegerType, BooleanType)
          case Less => (IntegerType, IntegerType, BooleanType)
          case Greater => (IntegerType, IntegerType, BooleanType)
          case Equals => (IntegerType, IntegerType, BooleanType)
          case Slash => (IntegerType, IntegerType, IntegerType)
          case Percent => (IntegerType, IntegerType, IntegerType)
          case Mul => (IntegerType, IntegerType, IntegerType)
          case Or => (BooleanType, BooleanType, BooleanType)
          case And => (BooleanType, BooleanType, BooleanType)
          case Mod => (IntegerType, IntegerType, IntegerType)
          case NotEquals => (IntegerType, IntegerType, BooleanType)
        }
        val lSubUnify = this.mgu(lType, lExpectedType)
        val lFinalSubst = this.composeSubst(lSubst, lSubUnify)
        val rSubUnify = this.mgu(rType, rExpectedType)
        val rFinalSubst = this.composeSubst(rSubst, rSubUnify)
        val finalSubst = this.composeSubst(lFinalSubst, rFinalSubst)
        (finalSubst, exprType)
      case UnaryOperation(operator, right) =>
        val (subRHS, rhsTy) = this.ti(typeEnv, right)
        val expectedTy = operator match {
          case Minus => IntegerType
          case Not => BooleanType
        }
        val subUnify = this.mgu(rhsTy, expectedTy)
        val finalSubst = this.composeSubst(subRHS, subUnify)
        (finalSubst, this.apply(rhsTy)(finalSubst))
      case Identifier(id) =>
        typeEnv.get(id) match {
          case Some((_, typeOf)) => (this.nullSubst, typeOf)
          case None => funTypeEnv.get(id) match {
            case Some(value) => (this.nullSubst, value._2)
            case None => throw AlgoTypeCheckingError(s"Variable '$id' non trouvé dans le scope actuel.")
          }
        }
      case FunctionCall(functionName, args) =>
        //TODO: repenser le design, je ne suis pas sur que tout ici soit à sa place, je pense que du code présent ici devrait plutôt se trouver dans typeInference
        val tVar = this.newTVar('t')
        funTypeEnv.get(functionName.value) match {
          case Some((ftvs, typeOf)) => typeOf match {
            case funType@FunctionType(parametersType, returnType) =>
              //On s'assure en premier qu'on a donné assez d'arguments et qu'on n'en donne pas trop.
              if(parametersType.size != args.size) throw AlgoTypeCheckingError(s"Erreur: Nombre d'arguments incorrect pour la fonction '${functionName.value}', elle en demande ${parametersType.size} mais en reçoit ${args.size}")
              //Ensuite on s'assure que les types des arguments données correspondent bien à ce qu'attends la fonction
              for((exprArg, expectedType) <- args zip parametersType) {
              }
              val (subst, exprType) = this.ti(typeEnv, functionName)
              println(s"functionType before instantiate: ${funType.showType()}")
              val functionType = this.instantiate(this.ftv(exprType).toList, exprType)
              println(s"functionType after instantiate: ${functionType.showType()}")
              val s3 = this.mgu(functionType, FunctionType(Seq(), tVar))
              (this.composeSubst(s3, subst), this.apply(tVar)(s3))
            case _ => throw AlgoTypeCheckingError(s"Erreur: Un type (${typeOf.showType()}) présent dans l'environnement de type des fonctions n'est pas un fonction.")
          }
          case None => throw AlgoTypeCheckingError(s"Fonction '${functionName.value}' non trouvé dans le scope.")
        }
    }
  }

  def typeInference(typeEnv: TypeEnv, expression: Expression): (Subst, Type) = {
    if(debugMode) println(s"TypeInference with typeEnv: $typeEnv and expr: $expression")
    val (subst, typeOf) = this.ti(typeEnv, expression)
    (subst, this.apply(typeOf)(subst))
  }

  def typeInference(block: Block, expectedType: Type): Unit = {
    var newTypeEnv = this.getCurrentTypeEnv
    block.instructions.foreach {
      case IfThenElseInstruction(cond, thenBlock, elseBlock) =>
        val (subst, typeOf) = this.typeInference(newTypeEnv, cond)
        val condSubst = this.mgu(typeOf, BooleanType)
        newTypeEnv = this.apply(newTypeEnv)(this.composeSubst(subst, condSubst))
        this.typeEnvStack.addOne(newTypeEnv)
        this.typeInference(thenBlock, UnitType)
        this.popEnv()
        elseBlock match {
          case Some(value) =>
            this.typeEnvStack.addOne(newTypeEnv)
            this.typeInference(value, UnitType)
            this.popEnv()
          case None =>
        }
      case WhileInstruction(cond, block) =>
        val (subst, typeOf) = this.typeInference(newTypeEnv, cond)
        val condSubst = this.mgu(typeOf, BooleanType)
        newTypeEnv = this.apply(newTypeEnv)(this.composeSubst(subst, condSubst))
        this.printTypeEnv(newTypeEnv)
        this.typeEnvStack.addOne(newTypeEnv)
        this.typeInference(block, UnitType)
        this.popEnv()
      case ForInstruction(id, from, to, block) =>
        val (exprFromSubst, fromType) = this.typeInference(newTypeEnv, from)
        val (exprToSubst, toType)     = this.typeInference(newTypeEnv, to)
        newTypeEnv.addOne((id.value, (List.empty, IntegerType)))
        val fromSubst = this.composeSubst(exprFromSubst, this.mgu(fromType, IntegerType))
        val toSubst   = this.composeSubst(exprToSubst, this.mgu(toType, IntegerType))
        newTypeEnv = this.apply(newTypeEnv)(this.mgu(toType, IntegerType))
        newTypeEnv = this.apply(newTypeEnv)(fromSubst)
        newTypeEnv = this.apply(newTypeEnv)(toSubst)
        this.printTypeEnv(newTypeEnv)
        this.typeEnvStack.addOne(newTypeEnv)
        this.typeInference(block, UnitType)
        this.popEnv()
      case Assignment(identifier, expression) =>
        val (subst, typeOf) = this.typeInference(newTypeEnv, expression)
        newTypeEnv = this.apply(newTypeEnv)(subst)
        newTypeEnv.addOne(identifier.value -> (List.empty[String], typeOf))
        this.popEnv()
        this.typeEnvStack.addOne(newTypeEnv)
      case Return(expression) =>
        expectedType match {
          case UnitType => throw AlgoTypeCheckingError("Erreur: Le bloc retourne quelque chose alors qu'il ne devrait pas.")
          case _ =>
            val (_, typeOf) = this.typeInference(newTypeEnv, expression)
            this.mgu(typeOf, expectedType)
            /*
            Je pense que cette ligne ne sert à rien puisque si l'algorithme n'arrive pas à trouver une substitution pour passer du type de l'expression au type que la fonction doit renvoyé, il va throw une erreur
            Il serait intéressant de réfléchir à une meilleur erreur à afficher dans ce contexte, c'est-à-dire que la fonction ne renvoit pas ce qu'elle doit renvoyer, on pourrait avoir une erreur plus précise pour ce cas particulier
             */
            //if(this.apply(typeOf)(subst) != expectedType) throw AlgoTypeCheckingError(s"Erreur: La fonction doit retourner une variable de type '${expectedType.showType()}' mais elle renvoie un type '${typeOf.showType()}'")
        }
      case ExprInstr(e) =>
    }
    //On va tester ici, c'est-à-dire quand on finis de typecheck un block s'il reste dans le typeenv associé à ce block une variable dont le type n'a pas été inféré
    //s'il en reste une alors sa déclaration est ambigüe et on le signale à l'utilisateur vià une erreur.
    this.getCurrentTypeEnv.foreach(
      {
        case (id, (_, typeOf)) => if(this.ftv(typeOf).nonEmpty) throw AlgoTypeCheckingError(s"Erreur: Impossible de déduire le type de la variable '$id' (${typeOf.showType()})")
    })
  }

  var funTypeEnv: TypeEnv = mutable.Map("lire"   -> (List.empty[String], FunctionType(Seq(), TVar("l0"))),
                                        "ecrire" -> (List.empty[String], FunctionType(Seq(TypeParameter(Identifier("elem"), AnyType(), In)), UnitType))
                                        )

  private val typeEnvStack: TypeEnvStack = mutable.ArrayBuffer(mutable.Map.empty[String, (List[String], Type)])
  private def getCurrentTypeEnv: TypeEnv = this.typeEnvStack.last
  type TypeEnvStack = mutable.ArrayBuffer[TypeEnv]

  def typeInference(program: Program): Unit = {
    program.declaredFunction.foreach(f => {
      val fId = f.declaration.functionName
      val fType = f.declaration.functionType
      println(s"On ajoute la fonction à l'env des fonctions: ${fType.showType()}")
      funTypeEnv += (fId.value -> (List.empty[String], fType))
      //On ajoute les types demandés par la fonction dans l'environnement de type actuel de manière à ce que ces variables soient accessible dans le corps de la fonction
      fType.parametersType.foreach(paramType => {
        this.getCurrentTypeEnv.addOne((paramType.name.value, (List.empty[String], paramType.paramType)))
      })
      this.typeInference(f.algo.block, fType.returnType)
    })
    this.typeInference(program.mainAlgo.block, UnitType)
  }

  private def popEnv(): Unit = {
    if(this.typeEnvStack.size == 1) return
    val actual: TypeEnv = this.getCurrentTypeEnv
    val parent = this.typeEnvStack(this.typeEnvStack.size-2)
    for((id, subst) <- actual)
      if(parent.contains(id)) {
        val parentScheme = parent(id)
        val newSubst: Subst = this.mgu(subst._2, parentScheme._2)
        parent.remove(id)
        parent.addOne(id, this.apply(parentScheme)(newSubst))
      }
  }

}
