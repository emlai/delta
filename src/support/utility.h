#pragma once

#include <cassert>
#include <iosfwd>
#include <string>
#include <utility> // std::move
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/raw_ostream.h>
#pragma warning(pop)
#include "../ast/location.h"

#ifndef NDEBUG
#define ASSERT(condition, ...) assert(condition)
#define ASSERT_EQUAL(a, b) \
    if (!((a) == (b))) { \
        llvm::errs() << "ASSERTION FAILED: " << #a << " == " << #b << " at " << __FILE__ << ":" << __LINE__ << '\n'; \
        DEBUG_PRINT(a); \
        DEBUG_PRINT(b); \
    }
#else
// Prevent unused variable warnings without evaluating the condition values.
#define ASSERT(condition, ...) ((void) sizeof(condition))
#define ASSERT_EQUAL(a, b) \
    ((void) sizeof(a)); \
    ((void) sizeof(b))
#endif

namespace delta {

struct Type;

std::ostream& operator<<(std::ostream& stream, llvm::StringRef string);

template<typename SourceContainer, typename Mapper>
auto map(const SourceContainer& source, Mapper mapper) -> std::vector<decltype(mapper(*source.begin()))> {
    std::vector<decltype(mapper(*source.begin()))> result;
    result.reserve(source.size());
    for (auto& element : source) {
        result.emplace_back(mapper(element));
    }
    return result;
}

template<typename T>
std::vector<T> instantiate(llvm::ArrayRef<T> elements, const llvm::StringMap<Type>& genericArgs) {
    return map(elements, [&](const T& element) { return element->instantiate(genericArgs); });
}

#define NOTNULL(x) (ASSERT(x), x)
#define DEBUG_PRINT(x) llvm::errs() << #x << " = " << (x) << '\n'

class StringFormatter : public llvm::raw_string_ostream {
public:
    StringFormatter() : llvm::raw_string_ostream(message) {}

private:
    std::string message;
};

std::string readLineFromFile(SourceLocation location);
void renameFile(llvm::Twine sourcePath, llvm::Twine targetPath);
void printDiagnostic(SourceLocation location, llvm::StringRef type, llvm::raw_ostream::Colors color, llvm::StringRef message);

struct Note {
    SourceLocation location;
    std::string message;
};

struct CompileError : std::exception {
    CompileError();
    CompileError(SourceLocation location, std::string&& message, std::vector<Note>&& notes = {});
    const char* what() const noexcept override { return message.c_str(); }
    void print() const;

    SourceLocation location;
    std::string message;
    std::vector<Note> notes;
};

template<typename T>
void printColored(const T& text, llvm::raw_ostream::Colors color) {
    if (llvm::outs().has_colors()) llvm::outs().changeColor(color, true);
    llvm::outs() << text;
    if (llvm::outs().has_colors()) llvm::outs().resetColor();
}

void printStackTrace();
[[noreturn]] void abort(StringFormatter& message);
void reportError(SourceLocation location, StringFormatter& message, llvm::ArrayRef<Note> notes = {});
void reportWarning(SourceLocation location, StringFormatter& message);

enum class WarningMode { Default, Suppress, TreatAsErrors };

#define ABORT(args) \
    { \
        printStackTrace(); \
        StringFormatter s; \
        s << args; \
        abort(s); \
    }

#define ERROR(location, args) \
    { \
        printStackTrace(); \
        StringFormatter s; \
        s << args; \
        throw CompileError(location, std::move(s.str())); \
    }

#define ERROR_WITH_NOTES(location, notes, args) \
    { \
        printStackTrace(); \
        StringFormatter s; \
        s << args; \
        throw CompileError(location, std::move(s.str()), notes); \
    }

#define REPORT_ERROR(location, args) \
    { \
        printStackTrace(); \
        StringFormatter s; \
        s << args; \
        reportError(location, s, {}); \
    }

#define REPORT_ERROR_WITH_NOTES(location, notes, args) \
    { \
        printStackTrace(); \
        StringFormatter s; \
        s << args; \
        reportError(location, s, notes); \
    }

#define WARN(location, args) \
    { \
        StringFormatter s; \
        s << args; \
        reportWarning(location, s); \
    }

std::string getCCompilerPath();

} // namespace delta
