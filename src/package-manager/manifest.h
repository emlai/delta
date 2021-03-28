#pragma once

#include <string>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>
#pragma warning(pop)

namespace delta {

struct PackageManifest {
    struct Dependency {
        Dependency(std::string&& packageIdentifier, std::string&& packageVersion)
        : packageIdentifier(std::move(packageIdentifier)), packageVersion(std::move(packageVersion)) {}
        std::string getGitRepositoryUrl() const;
        std::string getFileSystemPath() const;

        std::string packageIdentifier;
        std::string packageVersion;
    };

    PackageManifest(std::string&& packageRoot);
    std::vector<std::string> getTargetRootDirectories() const;
    static const char manifestFileName[];

    std::string packageRoot;
    std::string packageName;
    std::vector<Dependency> declaredDependencies;
    bool multitarget;
    std::string outputDirectory;
};

} // namespace delta
