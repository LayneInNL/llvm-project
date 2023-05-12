#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Format.h"

/* TODO: extract Type Info as types in ATerms
   TODO: transform some structures into sequences. For instnace, ReturnStmt
    contains a sequence of statements.
*/

using namespace clang;

class ATermsVisitor : public RecursiveASTVisitor<ATermsVisitor> {
public:
  explicit ATermsVisitor(ASTContext *Context) : Context(Context) {}

  // bool shouldTraversePostOrder() const { return true; }

  bool TraverseTranslationUnitDecl(TranslationUnitDecl *Decl) {
    llvm::outs() << "TranslationUnitDecl([";
    RecursiveASTVisitor<ATermsVisitor>::TraverseTranslationUnitDecl(Decl);
    llvm::outs() << "])";
    return true;
  }

  bool TraverseFunctionDecl(FunctionDecl *Decl) {
    llvm::outs() << "FunctionDecl(";
    VisitFunctionDecl(Decl);
    // RecursiveASTVisitor<ATermsVisitor>::TraverseFunctionDecl(Decl);
    {
      llvm::outs() << "[";
      bool HasFirst = true;
      for (ParmVarDecl *Parameter : Decl->parameters()) {
        if (HasFirst) {
          HasFirst = false;
        } else {
          llvm::outs() << ", ";
        }
        TraverseParmVarDecl(Parameter);
      }
      llvm::outs() << "], ";
    }
    TraverseStmt(Decl->getBody());
    llvm::outs() << ")";
    return true;
  }

  bool TraverseCompoundStmt(CompoundStmt *S) {
    llvm::outs() << S->getStmtClassName() << "(";
    {
      // llvm::outs() << "["; Comment it out now.
      for (Stmt *s : S->body()) {
        TraverseStmt(s);
      }
      // llvm::outs() << "]"; Comment it out now.
    }
    llvm::outs() << ")";

    return true;
  }

  bool VisitReturnStmt(ReturnStmt *Stmt) {
    llvm::outs() << Stmt->getStmtClassName() << "(";
    llvm::outs() << Stmt->getRetValue()->getStmtClassName() << "("
                 << Stmt->getRetValue()->EvaluateKnownConstInt(*Context) << ")";
    llvm::outs() << ")";
    return true;
  }

  // bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
  //   if (Declaration->getQualifiedNameAsString() == "n::m::C") {
  //     FullSourceLoc FullLocation =
  //     Context->getFullLoc(Declaration->getBeginLoc()); if
  //     (FullLocation.isValid())
  //       llvm::outs() << "Found declaration at "
  //                    << FullLocation.getSpellingLineNumber() << ":"
  //                    << FullLocation.getSpellingColumnNumber() << "\n";
  //   }
  //   return true;
  // }

  bool VisitTranslationUnitDecl(TranslationUnitDecl *Decl) {
    // llvm::outs() << "TranslationUnitDecl(";
    // for (auto subdecl : Decl->noload_decls()) {
    //   VisitDecl(subdecl);
    //   llvm::outs() << " ";
    // }
    // llvm::outs() << ")";
    return true;
  }

  bool VisitFunctionDecl(FunctionDecl *Decl) {
    // llvm::outs() << llvm::format("\"%s\"", Decl->getNameAsString());
    llvm::outs() << "\"" << Decl->getNameAsString() << "\", ";
    llvm::outs() << "Int(), ";
    // << Decl->getReturnType().getAsString();
    // llvm::outs() << "FunctionDecl([";
    // for (auto begin = Decl->param_begin(); begin != Decl->param_end();
    //      ++begin) {
    //   TraverseDecl(*begin);
    // }
    // llvm::outs() << "])";
    return true;
  }

  bool VisitParmVarDecl(ParmVarDecl *Decl) {
    llvm::outs() << "ParmVarDecl(\"" << Decl->getName() << "\", Int())";
    return true;
  }

private:
  ASTContext *Context;
};

class ATermsConsumer : public clang::ASTConsumer {
public:
  explicit ATermsConsumer(ASTContext *Context) : Visitor(Context) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    // Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    Visitor.TraverseAST(Context);

    // Visitor.VisitTranslationUnitDecl(Context.getTranslationUnitDecl());
    llvm::outs() << "\n";
  }

private:
  ATermsVisitor Visitor;
};

class ATermsAction : public clang::ASTFrontendAction {
public:
  virtual std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::make_unique<ATermsConsumer>(&Compiler.getASTContext());
  }
};

int main(int argc, char **argv) {
  clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(),
                                "int swap(int a, int b) { return 42; }");
  // clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(), "namespace
  // n {namespace m {class C{};}}");
}