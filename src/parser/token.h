#pragma once

#include <ostream>
#include <llvm/ADT/StringRef.h>
#include "../ast/srcloc.h"

namespace delta {

enum TokenKind {
    NO_TOKEN,
    IDENTIFIER,
    INT_LITERAL,
    FLOAT_LITERAL,
    STRING_LITERAL,
    BREAK, // Keywords...
    CASE,
    CAST,
    CLASS,
    CONST,
    DEFAULT,
    DEFER,
    DEINIT,
    ELSE,
    EXTERN,
    FALSE,
    FUNC,
    IF,
    IMPORT,
    INIT,
    MUTABLE,
    NULL_LITERAL,
    RETURN,
    STRUCT,
    SWITCH,
    THIS,
    TRUE,
    UNINITIALIZED,
    VAR,
    WHILE,
    UNDERSCORE,
    EQ, // Operators...
    NE,
    LT,
    LE,
    GT,
    GE,
    PLUS,
    PLUS_EQ,
    MINUS,
    MINUS_EQ,
    STAR,
    STAR_EQ,
    SLASH,
    SLASH_EQ,
    INCREMENT,
    DECREMENT,
    NOT,
    AND,
    AND_EQ,
    AND_AND,
    AND_AND_EQ,
    OR,
    OR_EQ,
    OR_OR,
    OR_OR_EQ,
    XOR,
    XOR_EQ,
    COMPL,
    LSHIFT,
    LSHIFT_EQ,
    RSHIFT,
    RSHIFT_EQ,
    ASSIGN, // Miscellaneous...
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    DOT,
    COMMA,
    COLON,
    COLON_COLON,
    SEMICOLON,
    RARROW,
    TOKEN_COUNT
};

struct Token {
    const TokenKind kind;
    llvm::StringRef string; ///< The substring in the source code representing this token.

    Token(TokenKind kind, llvm::StringRef string = {});
    operator TokenKind() const { return kind; }
    SrcLoc getLoc() const { return srcLoc; }
    bool is(TokenKind kind) const { return this->kind == kind; }
    template<typename... T>
    bool is(TokenKind kind, T... kinds) const { return is(kind) || is(kinds...); }
    bool isBinaryOperator() const;
    bool isPrefixOperator() const;
    int getPrecedence() const;
    int64_t getIntegerValue() const;
    long double getFloatingPointValue() const;

    /// Strips the trailing '=' from a compound assignment operator.
    /// E.g. given '+=', returns '+', and so on.
    Token withoutCompoundEqSuffix() const { return Token(TokenKind(kind - 1)); }

private:
    const SrcLoc srcLoc;
};

struct PrefixOperator {
    PrefixOperator(Token token);
    operator TokenKind() const { return kind; }
    TokenKind kind;
};

struct BinaryOperator {
    BinaryOperator(Token token);
    operator TokenKind() const { return kind; }
    bool isComparisonOperator() const;
    bool isBitwiseOperator() const;
    TokenKind kind;
};

std::ostream& operator<<(std::ostream& stream, TokenKind tokenKind);

}