#pragma once

#include "mull/JunkDetection/CXX/ASTStorage.h"
#include "mull/Filters/FilePathFilter.h"

#include <clang/AST/RecursiveASTVisitor.h>

#include <iostream>

namespace mull {

class ASTVisitor : public clang::RecursiveASTVisitor<ASTVisitor> {
  mull::ThreadSafeASTUnit &astUnit;
  mull::ASTStorage &astStorage;
  clang::SourceManager &sourceManager;
  mull::FilePathFilter &filePathFilter;
  mull::MutatorKindSet mutatorKindSet;
  bool shouldSkipCurrentFunction;

public:
  explicit ASTVisitor(mull::ThreadSafeASTUnit &astUnit,
                      mull::ASTStorage &astStorage,
                      mull::FilePathFilter &filePathFilter,
                      mull::MutatorKindSet mutatorKindSet);

  bool VisitFunctionDecl(clang::FunctionDecl *Decl);
  bool VisitExpr(clang::Expr *expr);

  void traverse();

private:
  void saveMutationPoint(mull::MutatorKind mutatorKind,
                         const clang::Stmt *stmt,
                         clang::SourceLocation location);
};

} // namespace mull
