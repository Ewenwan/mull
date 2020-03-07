#include "mull/AST/ASTVisitor.h"

#include "mull/Program/Program.h"

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Basic/SourceLocation.h>
#include <llvm/IR/Module.h>

#include <cassert>

using namespace mull;

static std::vector<std::pair<clang::BinaryOperator::Opcode, mull::MutatorKind>> BINARY_MUTATIONS = {
  /// CXX Arithmetic

  { clang::BO_Add, mull::MutatorKind::CXX_AddToSub },
  { clang::BO_Sub, mull::MutatorKind::CXX_SubToAdd },
  { clang::BO_Mul, mull::MutatorKind::CXX_MulToDiv },
  { clang::BO_Rem, mull::MutatorKind::CXX_RemToDiv },
  { clang::BO_Div, mull::MutatorKind::CXX_DivToMul },

  { clang::BO_AddAssign, mull::MutatorKind::CXX_AddAssignToSubAssign },
  { clang::BO_SubAssign, mull::MutatorKind::CXX_SubAssignToAddAssign },
  { clang::BO_MulAssign, mull::MutatorKind::CXX_MulAssignToDivAssign },
  { clang::BO_RemAssign, mull::MutatorKind::CXX_RemAssignToDivAssign },
  { clang::BO_DivAssign, mull::MutatorKind::CXX_DivAssignToMulAssign },

  /// Relational

  { clang::BO_EQ, mull::MutatorKind::CXX_EqualToNotEqual },
  { clang::BO_NE, mull::MutatorKind::CXX_NotEqualToEqual },

  { clang::BO_GT, mull::MutatorKind::CXX_GreaterThanToLessOrEqual },
  { clang::BO_LT, mull::MutatorKind::CXX_LessThanToGreaterOrEqual },

  /// Bitwise

  { clang::BO_Shl, mull::MutatorKind::CXX_LShiftToRShift },
  { clang::BO_Shr, mull::MutatorKind::CXX_RShiftToLShift },

  /// AND-OR

  { clang::BO_LAnd, mull::MutatorKind::CXX_Logical_AndToOr },
  { clang::BO_LOr, mull::MutatorKind::CXX_Logical_OrToAnd },
};

static bool isConstant(clang::Expr *statement) {
  statement = statement->IgnoreImplicit();
  if (clang::isa<clang::IntegerLiteral>(statement)) {
    return true;
  }
  if (clang::isa<clang::FloatingLiteral>(statement)) {
    return true;
  }

  if (clang::isa<clang::CharacterLiteral>(statement)) {
    return true;
  }
  if (const clang::DeclRefExpr *declRefExpr = clang::dyn_cast<clang::DeclRefExpr>(statement)) {
    const clang::Decl *referencedDecl = declRefExpr->getReferencedDeclOfCallee();
    if (clang::isa<clang::EnumConstantDecl>(referencedDecl)) {
      return true;
    }
  }
  return false;
}

static bool findPotentialMutableParentStmt(const clang::Stmt *statement,
                                           clang::ASTContext &astContext,
                                           clang::SourceManager &sourceManager,
                                           clang::SourceLocation *mutationLocation) {
  assert(mutationLocation);

  for (auto &parent : astContext.getParents(*statement)) {
    if (const clang::BinaryOperator *binaryOpParent = parent.get<clang::BinaryOperator>()) {
      *mutationLocation = binaryOpParent->getOperatorLoc();
      return true;
    }

    if (const clang::ReturnStmt *returnParent = parent.get<clang::ReturnStmt>()) {
      *mutationLocation = returnParent->getBeginLoc();
      return true;
    }

    if (const clang::CXXMemberCallExpr *callExpr = parent.get<clang::CXXMemberCallExpr>()) {
      *mutationLocation = callExpr->getExprLoc();
      return true;
    }

    if (const clang::CallExpr *callExpr = parent.get<clang::CallExpr>()) {
      *mutationLocation = callExpr->getBeginLoc();
      return true;
    }

    if (const clang::ImplicitCastExpr *castExpr = parent.get<clang::ImplicitCastExpr>()) {
      return findPotentialMutableParentStmt(castExpr, astContext, sourceManager, mutationLocation);
    }

    if (const clang::CStyleCastExpr *castExpr = parent.get<clang::CStyleCastExpr>()) {
      return findPotentialMutableParentStmt(castExpr, astContext, sourceManager, mutationLocation);
    }

    // TODO: Not implemented
    if (const clang::ConstantExpr *constantExpr = parent.get<clang::ConstantExpr>()) {
      return false;
    }

    if (const clang::CXXConstructorDecl *cxxConstructorDecl =
            parent.get<clang::CXXConstructorDecl>()) {

      for (auto &member : cxxConstructorDecl->inits()) {
        if (member->getInit() == statement) {
          *mutationLocation = member->getMemberLocation();
          return true;
        }
      }
    }

    if (const clang::VarDecl *varDecl = parent.get<clang::VarDecl>()) {
      *mutationLocation = varDecl->getLocation();
      return true;
    }

    if (const clang::CXXTemporaryObjectExpr *cxxTemporaryObjectExpr =
            parent.get<clang::CXXTemporaryObjectExpr>()) {
      *mutationLocation = cxxTemporaryObjectExpr->getExprLoc();
      return true;
    }

    // TODO: Not implemented on the LLVM IR level.
    if (const clang::ArraySubscriptExpr *arraySubscriptExpr =
            parent.get<clang::ArraySubscriptExpr>()) {
      *mutationLocation = arraySubscriptExpr->getExprLoc();
      return false;
    }

    // TODO: Not implemented
    llvm::errs() << "\n";
    statement->dump(llvm::errs(), sourceManager);
    parent.dump(llvm::errs(), sourceManager);
    //    assert(0);

    return false;
  }

  return false;
}

ASTVisitor::ASTVisitor(mull::ThreadSafeASTUnit &astUnit, mull::ASTStorage &astStorage,
                       mull::FilePathFilter &filePathFilter, mull::MutatorKindSet mutatorKindSet)
    : astUnit(astUnit), astStorage(astStorage), filePathFilter(filePathFilter),
      sourceManager(astUnit.getSourceManager()), mutatorKindSet(mutatorKindSet),
      shouldSkipCurrentFunction(false) {}

bool ASTVisitor::VisitFunctionDecl(clang::FunctionDecl *Decl) {
  if (Decl->getNameAsString() == "main") {
    shouldSkipCurrentFunction = true;
  } else {
    shouldSkipCurrentFunction = false;
  }
  return clang::RecursiveASTVisitor<ASTVisitor>::VisitFunctionDecl(Decl);
}

bool ASTVisitor::VisitExpr(clang::Expr *expr) {
  if (shouldSkipCurrentFunction) {
    return true;
  }

  clang::SourceLocation exprLocation = expr->getBeginLoc();
  if (astUnit.isInSystemHeader(exprLocation)) {
    return true;
  }

  const std::string sourceLocation = astUnit.getSourceManager().getFilename(exprLocation).str();

  if (sourceLocation.empty()) {
    /// TODO: asserts only?
    return true;
  }

  if (filePathFilter.shouldSkip(sourceLocation)) {
    return true;
  }

  /// In case of PredefinedExpr, the expression expr->children() yields
  /// children which are nullptr. These nodes should be junk anyway, so we
  /// ignore them early.
  if (clang::isa<clang::PredefinedExpr>(expr)) {
    return true;
  }

  /// Binary operations

  if (clang::BinaryOperator *binaryOperator = clang::dyn_cast<clang::BinaryOperator>(expr)) {

    clang::SourceLocation binaryOperatorLocation = binaryOperator->getOperatorLoc();

    for (const std::pair<clang::BinaryOperator::Opcode, mull::MutatorKind> &mutation :
         BINARY_MUTATIONS) {
      if (binaryOperator->getOpcode() == mutation.first &&
          mutatorKindSet.includesMutator(mutation.second)) {

        for (auto &parent : astUnit.getASTContext().getParents(*binaryOperator)) {
          /// This case removes binary operators that are parts of template
          /// arguments, for example:
          /// return variant_index<VariantType, T, index + 1>();
          /// TODO: Some other edge cases can hide behind this though and we
          /// might want to whitelist them back later.
          if (parent.get<clang::UnresolvedLookupExpr>()) {
            return true;
          }
        }

        saveMutationPoint(mutation.second, binaryOperator, binaryOperatorLocation);
        return true;
      }
    }

    return true;
  }

  /// Replace Call

  if (mutatorKindSet.includesMutator(MutatorKind::ReplaceCallMutator)) {
    if (clang::isa<clang::CallExpr>(expr)) {
      saveMutationPoint(mull::MutatorKind::ReplaceCallMutator, expr, expr->getBeginLoc());

      return true;
    }
  }

  /// Negate

  if (mutatorKindSet.includesMutator(MutatorKind::NegateMutator)) {
    if (clang::isa<clang::ImplicitCastExpr>(expr)) {
      return true;
    }

    if (clang::DeclRefExpr *declRefExpr = clang::dyn_cast<clang::DeclRefExpr>(expr)) {
      for (auto &parent : astUnit.getASTContext().getParents(*declRefExpr)) {
        if (const clang::UnaryOperator *unaryOperator = parent.get<clang::UnaryOperator>()) {
          if (clang::Expr *subExpr = unaryOperator->getSubExpr()) {
            saveMutationPoint(
                mull::MutatorKind::NegateMutator, unaryOperator, subExpr->getExprLoc());
            return true;
          }
        }
      }

      // TODO does this have to be here? CALL_EXPRESSION?
      saveMutationPoint(mull::MutatorKind::NegateMutator, declRefExpr, declRefExpr->getExprLoc());
    }

    return true;
  }

  /// Scalar

  if (mutatorKindSet.includesMutator(MutatorKind::ScalarValueMutator) && isConstant(expr)) {
    expr = expr->IgnoreImplicit();

    //    expr = expr->Ignore();
    /// The constant literals do not have their own debug information in the
    /// LLVM IR if they are part of a parent instruction, such as binary
    /// operator, return instruction, call expr, etc.
    /// Therefore we check for the parent nodes that can result in mutable IR
    /// instructions. We record the parent node' location instead of the
    /// constant location.

    clang::SourceLocation mutationLocation;
    bool foundMutableParent = findPotentialMutableParentStmt(
        expr, astUnit.getASTContext(), sourceManager, &mutationLocation);

    if (foundMutableParent) {
      saveMutationPoint(mull::MutatorKind::ScalarValueMutator, expr, mutationLocation);
      return true;
    }

    // parent.dump(llvm::errs(), sourceManager);

    /// TODO: STAN
    /// assert(0 && "Should not reach here");
  }

  /// Remove Void

  if (const clang::CallExpr *callExpr = clang::dyn_cast<clang::CallExpr>(expr)) {
    if (mutatorKindSet.includesMutator(MutatorKind::RemoveVoidFunctionMutator)) {
      auto *type = callExpr->getType().getTypePtrOrNull();
      if (type && type->isVoidType()) {
        saveMutationPoint(
            mull::MutatorKind::RemoveVoidFunctionMutator, callExpr, callExpr->getBeginLoc());
      }
      return true;
    }
  }

  return true;
}

void ASTVisitor::traverse() {
  clang::ASTContext &context = astUnit.getASTContext();
  TraverseDecl(context.getTranslationUnitDecl());
}

void ASTVisitor::saveMutationPoint(mull::MutatorKind mutatorKind, const clang::Stmt *stmt,
                                   clang::SourceLocation location) {

  int beginLine = sourceManager.getExpansionLineNumber(location, nullptr);
  int beginColumn = sourceManager.getExpansionColumnNumber(location);

  std::string sourceFilePath = astUnit.getSourceManager().getFilename(location).str();
  if (sourceFilePath.size() == 0) {
    /// we reach here because of asserts()
    /// TODO: maybe there are more cases.
    return;
  }

  if (!llvm::sys::fs::is_regular_file(sourceFilePath) && sourceFilePath != "input.cc") {
    llvm::errs() << "error: ASTVisitor: invalid source file path: '" << sourceFilePath << "'\n";

    exit(1);
  }

  astStorage.saveMutation(sourceFilePath, mutatorKind, stmt, beginLine, beginColumn);
}
