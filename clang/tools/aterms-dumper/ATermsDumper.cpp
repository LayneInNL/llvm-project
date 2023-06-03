#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormatVariadic.h"

/* TODO: extract Type Info as types in ATerms
   TODO: transform some structures into sequences. For instnace, ReturnStmt
    contains a sequence of statements.
*/

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
  llvm::formatv("{0}, {1}, {2}, {3}, {4}", FileName, IncludeTok.getName(),
                IsAngled, FilenameRange.getAsRange().printToString(SM));
}

class ATermsVisitor : public clang::RecursiveASTVisitor<ATermsVisitor> {
public:
  explicit ATermsVisitor(clang::ASTContext *Context) : Context(Context) {}

  // traverse in post order if the return is true
  // bool shouldTraversePostOrder() const { return true; }

  // Start dumping from a translation unit
  bool TraverseTranslationUnitDecl(clang::TranslationUnitDecl *Decl) {
    llvm::outs() << llvm::formatv("TranslationUnitDecl([");
    // call the function of the parent class to perform dumping
    clang::RecursiveASTVisitor<ATermsVisitor>::TraverseTranslationUnitDecl(
        Decl);
    llvm::outs() << llvm::formatv("])");
    return true;
  }

  bool TraverseFunctionDecl(clang::FunctionDecl *Decl) {
    llvm::outs() << llvm::formatv("FunctionDecl(\"{0}\", Int(), ");
    // Parameters
    {
      llvm::outs() << llvm::formatv("[");
      bool HasFirst = true;
      for (clang::ParmVarDecl *Parameter : Decl->parameters()) {
        if (HasFirst) {
          HasFirst = false;
        } else {
          llvm::outs() << llvm::formatv(", ");
        }
        TraverseParmVarDecl(Parameter);
      }
      llvm::outs() << llvm::formatv("], ");
    }
    TraverseStmt(Decl->getBody());
    llvm::outs() << llvm::formatv(")");
    return true;
  }

  bool TraverseCompoundStmt(clang::CompoundStmt *S) {
    llvm::outs() << llvm::formatv("{0}(", S->getStmtClassName());
    {
      // llvm::outs() << "["; Comment it out now.
      for (clang::Stmt *s : S->body()) {
        TraverseStmt(s);
      }
      // llvm::outs() << "]"; Comment it out now.
    }
    llvm::outs() << llvm::formatv(")");

    return true;
  }

  bool VisitReturnStmt(clang::ReturnStmt *Stmt) {
    llvm::outs() << llvm::formatv(
        "{0}({1}({2}))", Stmt->getStmtClassName(),
        Stmt->getRetValue()->getStmtClassName(),
        Stmt->getRetValue()->EvaluateKnownConstInt(*Context));
    return true;
  }

  bool VisitTranslationUnitDecl(clang::TranslationUnitDecl *Decl) {
    return true;
  }

  bool VisitParmVarDecl(clang::ParmVarDecl *Decl) {
    clang::SourceRange SR = Decl->getSourceRange();
    clang::SourceLocation Start = SR.getBegin(), End = SR.getEnd();
    clang::FullSourceLoc PStart = clang::FullSourceLoc(
                             Start, Context->getSourceManager()),
                         PEnd = clang::FullSourceLoc(
                             End, Context->getSourceManager());
    llvm::outs() << llvm::formatv(
        "ParmVarDecl(\"Decl->getName()\", Int()) {Loc({0},{1})}",
        PStart.getFileOffset(), PEnd.getFileOffset());

    return true;
  }

private:
  clang::ASTContext *Context;
};

class ATermsConsumer : public clang::ASTConsumer {
public:
  explicit ATermsConsumer(clang::ASTContext *Context) : Visitor(Context) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    // Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    clang::SourceManager &SM = Context.getSourceManager();
    clang::FileID MainFileID = SM.getMainFileID();
    const clang::FileEntry *FE = SM.getFileEntryForID(MainFileID);
    llvm::outs() << llvm::formatv("SourceFile(\"{0}\", ", FE->getName());
    // Start traversing
    Visitor.TraverseAST(Context);
    llvm::outs() << llvm::formatv(")\n");
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
    // clang::HeaderSearchOptions &HSO = CI.getHeaderSearchOpts();
    // HSO.AddPath("/Users/layneliu/Projects/llvm-project/build/include/c++/v1",
    //             clang::frontend::IncludeDirGroup::CXXSystem, false, false);
    // HSO.AddPath("/Users/layneliu/Projects/macros",
    //             clang::frontend::IncludeDirGroup::Quoted, false, false);
    // for (auto one : CI.getHeaderSearchOpts().SystemHeaderPrefixes) {
    //   llvm::outs() << one.Prefix;
    // }
    // HSO.ResourceDir =
    //     "/Users/layneliu/Projects/llvm-project/build/lib/clang/16";
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

int main(int argc, const char **argv) {
  // clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(),
  // "int swap(int a, int b) { return 42; }",
  // "casestudy.cpp");
  // clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(), "namespace
  // n {namespace m {class C{};}}");
  // clang::tooling::runToolOnCode(std::make_unique<ATermsAction>(),
  //                               "#include \"swapsig.h\"\n", "swapsig.cpp");
  llvm::cl::OptionCategory OC = llvm::cl::OptionCategory("ATerms Dumper");
  const llvm::opt::OptTable &Options = clang::driver::getDriverOptTable();
  llvm::Expected<clang::tooling::CommonOptionsParser> ECOP =
      clang::tooling::CommonOptionsParser::create(argc, argv, OC);
  if (!ECOP) {
    llvm::errs() << ECOP.takeError();
    return 1;
  }
  clang::tooling::CommonOptionsParser &COP = ECOP.get();

  clang::tooling::ClangTool Tool =
      clang::tooling::ClangTool(COP.getCompilations(), COP.getSourcePathList());
  std::unique_ptr<clang::tooling::FrontendActionFactory> FAF =
      clang::tooling::newFrontendActionFactory<ATermsAction>();
  Tool.run(FAF.get());
}