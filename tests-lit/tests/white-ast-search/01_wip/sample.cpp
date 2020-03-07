int sum(int a, int b) {
  return a + b;
}

int main() {
  return sum(-2, 2);
}

/**
; RUN: cd / && %CLANG_EXEC -fembed-bitcode -g -grecord-command-line %s -o %s.exe
; RUN: cd %CURRENT_DIR
; RUN: (unset TERM; %MULL_EXEC -enable-ast -test-framework CustomTest -mutators=cxx_add_to_sub -reporters=IDE -ide-reporter-show-killed %s.exe 2>&1; test $? = 0) | %FILECHECK_EXEC %s --strict-whitespace --match-full-lines
; CHECK:[info] Found compilation flags in the input bitcode
; CHECK-NOT:{{^.*[Ee]rror.*$}}
; CHECK:{{^.*}}sample.cpp:2:12: warning: Killed: Replaced + with - [cxx_add_to_sub]{{$}}
; CHECK:  return a + b;
; CHECK:           ^
; CHECK:[info] Mutation score: 100%
; CHECK-EMPTY:
**/
