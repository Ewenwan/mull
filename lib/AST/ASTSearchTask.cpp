#include "mull/AST/ASTSearchTask.h"

#include "mull/AST/ASTVisitor.h"
#include "mull/Parallelization/Progress.h"

using namespace mull;

ASTSearchTask::ASTSearchTask(mull::Diagnostics &diagnostics, mull::ASTStorage &astStorage,
                             mull::MutatorKindSet mutatorKindSet, mull::FilePathFilter &pathFilter)
    : diagnostics(diagnostics), astStorage(astStorage), mutatorKindSet(mutatorKindSet),
      pathFilter(pathFilter) {}

void ASTSearchTask::operator()(iterator begin, iterator end, Out &storage,
                               mull::progress_counter &counter) {

  for (auto it = begin; it != end; it++, counter.increment()) {
    const std::unique_ptr<mull::Bitcode> &bitcode = *it;

    const llvm::Module *const module = bitcode->getModule();
    std::string sourceFileName = module->getSourceFileName();
    if (pathFilter.shouldSkip(sourceFileName)) {
      diagnostics.info(std::string("ASTSearchTask: skipping path: ") + sourceFileName);
      continue;
    }

    if (!llvm::sys::fs::is_regular_file(sourceFileName) && sourceFileName != "input.cc") {
      diagnostics.error(std::string("ASTSearchTask: invalid source file path: ") + sourceFileName);
      exit(1);
    }

    auto ast = astStorage.findAST(sourceFileName);
    ASTVisitor visitor(*ast, astStorage, pathFilter, mutatorKindSet);
    visitor.TraverseDecl(ast->getASTContext().getTranslationUnitDecl());
  }
}
