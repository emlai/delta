#include "manifest.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/Process.h>
#include "../ast/module.h"
#include "../driver/driver.h"
#include "../parser/parse.h"
#include "../support/utility.h"

using namespace delta;

const char PackageManifest::manifestFileName[] = "package.delta";

std::string PackageManifest::Dependency::getGitRepositoryUrl() const {
    return "https://github.com/" + packageIdentifier + ".git";
}

std::string PackageManifest::Dependency::getFileSystemPath() const {
    auto home = llvm::sys::Process::GetEnv("HOME");
    if (!home) {
        ABORT("environment variable HOME not set");
    }
    return *home + "/.delta/dependencies/" + packageIdentifier + "@" + packageVersion;
}

template<typename DeclT, typename DefaultValueT>
static auto getConfigValue(Decl* decl, DefaultValueT defaultValue) {
    return decl ? llvm::cast<DeclT>(llvm::cast<VarDecl>(decl)->initializer)->getValue() : defaultValue;
}

PackageManifest::PackageManifest(std::string&& packageRoot) : packageRoot(std::move(packageRoot)) {
    auto manifestPath = this->packageRoot + "/" + manifestFileName;
    if (!llvm::sys::fs::exists(manifestPath)) return;

    Module module(manifestFileName);
    CompileOptions options;
    Parser parser(manifestPath, module, options);
    parser.parse();
    // TODO: Type-check package manifest.

    auto& symbols = module.getSymbolTable();
    packageName = getConfigValue<StringLiteralExpr>(symbols.findOne("name"), "").str();
    multitarget = getConfigValue<BoolLiteralExpr>(symbols.findOne("multitarget"), false);
    outputDirectory = getConfigValue<StringLiteralExpr>(symbols.findOne("outputDirectory"), "bin").str();

    if (auto* dependencies = symbols.findOne("dependencies")) {
        auto* array = llvm::cast<ArrayLiteralExpr>(llvm::cast<VarDecl>(dependencies)->initializer);
        for (auto& element : array->getElements()) {
            auto* tuple = llvm::cast<TupleExpr>(&*element);
            auto* package = llvm::cast<StringLiteralExpr>(tuple->getElementByName("package"));
            auto* version = llvm::cast<StringLiteralExpr>(tuple->getElementByName("version"));
            declaredDependencies.push_back(Dependency(package->getValue().str(), version->getValue().str()));
        }
    }
}

std::vector<std::string> PackageManifest::getTargetRootDirectories() const {
    if (!multitarget) return { packageRoot };

    std::string sourceDir = packageRoot;
    std::error_code error;

    for (llvm::sys::fs::directory_iterator it(packageRoot, error), end; it != end; it.increment(error)) {
        if (!llvm::sys::fs::is_directory(it->path())) continue;
        llvm::StringRef dir = llvm::sys::path::filename(it->path());
        if (dir.equals_lower("src") || dir.equals_lower("source") || dir.equals_lower("sources")) {
            sourceDir += llvm::sys::path::get_separator();
            sourceDir += dir;
            break;
        }
    }

    std::vector<std::string> targetDirs;

    for (llvm::sys::fs::directory_iterator it(sourceDir, error), end; it != end; it.increment(error)) {
        if (llvm::sys::fs::is_directory(it->path())) {
            targetDirs.push_back(it->path());
        }
    }

    return targetDirs;
}
