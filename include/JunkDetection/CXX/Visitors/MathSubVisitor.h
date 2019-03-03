#pragma once

#include "JunkDetection/CXX/Visitors/InstructionRangeVisitor.h"
#include "JunkDetection/CXX/Visitors/VisitorParameters.h"

#include <clang/AST/RecursiveASTVisitor.h>

namespace mull {

class MathSubVisitor : public clang::RecursiveASTVisitor<MathSubVisitor> {
public:
  MathSubVisitor(const VisitorParameters &parameters);

  bool VisitBinaryOperator(clang::BinaryOperator *binaryOperator);
  bool VisitUnaryOperator(clang::UnaryOperator *unaryOperator);
  bool foundMutant();

private:
  InstructionRangeVisitor visitor;
};

} // namespace mull
