#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/Format.h"

/* TODO: extract Type Info as types in ATerms
   TODO: transform some structures into sequences. For instnace, ReturnStmt
    contains a sequence of statements.
*/

using namespace clang;

class IncludesExtractor : public clang::PPCallbacks {
public:
  void InclusionDirective(clang::SourceLocation HashLoc,
                          const clang::Token &IncludeTok,
                          clang::StringRef FileName, bool IsAngled,
                          clang::CharSourceRange FilenameRange,
                          clang::OptionalFileEntryRef File,
                          clang::StringRef SearchPath,
                          clang::StringRef RelativePath,
                          const clang::Module *Imported,
                          clang::SrcMgr::CharacteristicKind FileType) override;
  IncludesExtractor(clang::SourceManager &SM) : SM(SM) {}

private:
  clang::SourceManager &SM;
};

void IncludesExtractor::InclusionDirective(
    clang::SourceLocation HashLoc, const clang::Token &IncludeTok,
    clang::StringRef FileName, bool IsAngled,
    clang::CharSourceRange FilenameRange, clang::OptionalFileEntryRef File,
    clang::StringRef SearchPath, clang::StringRef RelativePath,
    const clang::Module *Imported, clang::SrcMgr::CharacteristicKind FileType) {
  llvm::outs() << FileName << "\n";
  clang::FullSourceLoc FSL = clang::FullSourceLoc(HashLoc, SM);
  llvm::outs() << IncludeTok.getName() << "\n";
  llvm::outs() << IsAngled << FilenameRange.getAsRange().printToString(SM)
               << "\n";
}

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
    // clang::SourceRange SR = Decl->getSourceRange();
    // bool x = false;
    // llvm::outs()
    //     << "###"
    //     << Context->getSourceManager().getCharacterData(SR.getBegin(), &x)
    //     << "###"
    //     << Context->getSourceManager().isMacroBodyExpansion(SR.getBegin())
    //     << Context->getSourceManager().isMacroArgExpansion(SR.getBegin());
    // clang::SourceLocation SL = Decl->getLocation();
    // clang::SourceLocation ISL =
    //     Context->getSourceManager().getImmediateSpellingLoc(SL);
    // llvm::outs() << Context->getSourceManager().getCharacterData(ISL);
    return true;
  }

  bool VisitParmVarDecl(ParmVarDecl *Decl) {
    llvm::outs() << "ParmVarDecl(\"" << Decl->getName() << "\", Int())";
    clang::SourceRange SR = Decl->getSourceRange();
    clang::SourceLocation Start = SR.getBegin();
    clang::FullSourceLoc PStart =
        clang::FullSourceLoc(Start, Context->getSourceManager());
    clang::SourceLocation End = SR.getEnd();
    clang::FullSourceLoc PEnd =
        clang::FullSourceLoc(End, Context->getSourceManager());
    llvm::outs() << " {Loc(" << PStart.getFileOffset() << ","
                 << PEnd.getFileOffset() << ")} ";
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
    clang::SourceManager &SM = Context.getSourceManager();
    clang::FileID MainFileID = SM.getMainFileID();
    const clang::FileEntry *FE = SM.getFileEntryForID(MainFileID);
    llvm::outs() << "SourceFile(\"" << FE->getName() << "\", ";
    Visitor.TraverseAST(Context);
    llvm::outs() << ")";
    llvm::outs() << "\n";
    // // Visitor.VisitTranslationUnitDecl(Context.getTranslationUnitDecl());
    // for (auto begin = SM.fileinfo_begin(); begin != SM.fileinfo_end();
    //      begin++) {
    //   llvm::outs() << "filename: " << begin->first->getName() << "\n";
    //   llvm::outs() << "isMainFile: " << SM.isMainFile(*(begin->first)) <<
    //   "\n"; clang::FileID FID = SM.translateFile(begin->first);
    //   clang::SourceLocation StartSL = SM.getLocForStartOfFile(FID);
    //   llvm::outs() << "loaction: " << StartSL.printToString(SM) << "\n";
    //   clang::SourceLocation EndSL = SM.getLocForEndOfFile(FID);
    //   // for (; StartSL != EndSL;) {
    //   bool Invalid;
    //   auto s = SM.getCharacterData(StartSL, &Invalid);
    //   llvm::outs() << "default: " << s;
    //   clang::SourceLocation SpellingSL = SM.getSpellingLoc(StartSL);
    //   s = SM.getCharacterData(SpellingSL, &Invalid);
    //   llvm::outs() << "spelling: " << s;
    //   clang::SourceLocation ExpantionSL = SM.getExpansionLoc(StartSL);
    //   s = SM.getCharacterData(ExpantionSL, &Invalid);
    //   llvm::outs() << "expansion: " << s;
    //   // StartSL = StartSL + 1;
    //   // }
    // }
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

  virtual bool BeginInvocation(clang::CompilerInstance &CI) {
    clang::LangOptions &LO = CI.getLangOpts();
    LO.CPlusPlus = 1;
    LO.CPlusPlus17 = 1;
    clang::HeaderSearchOptions &HSO = CI.getHeaderSearchOpts();
    HSO.AddPath("/Users/layneliu/Projects/llvm-project/build/include/c++/v1",
                clang::frontend::IncludeDirGroup::CXXSystem, false, false);
    HSO.AddPath("/Users/layneliu/Projects/macros",
                clang::frontend::IncludeDirGroup::Quoted, false, false);
    // HSO.AddSystemHeaderPrefix("/usr/local/include", true);
    // HSO.AddSystemHeaderPrefix(
    //     "/Users/layneliu/Projects/llvm-project/build/lib/clang/16/include",
    //     true);
    // HSO.AddSystemHeaderPrefix(
    //     "/Users/layneliu/Projects/llvm-project/build/lib/clang/16/"
    //     "include",
    //     true);
    for (auto one : CI.getHeaderSearchOpts().SystemHeaderPrefixes) {
      llvm::outs() << one.Prefix;
    }
    HSO.ResourceDir =
        "/Users/layneliu/Projects/llvm-project/build/lib/clang/16";
    return true;
  }

  bool BeginSourceFileAction(clang::CompilerInstance &CI) {
    clang::Preprocessor &PP = CI.getPreprocessor();
    std::unique_ptr<IncludesExtractor> IncludesExtractorCallback(
        new IncludesExtractor(CI.getSourceManager()));
    PP.addPPCallbacks(std::move(IncludesExtractorCallback));

    return true;
  }
};

int main(int argc, char **argv) {
  // clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(),
  // "int swap(int a, int b) { return 42; }",
  // "casestudy.cpp");
  // clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(), "namespace
  // n {namespace m {class C{};}}");
  clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(),
                                "#include \"swapsig.h\"\n", "swapsig.cpp");
}