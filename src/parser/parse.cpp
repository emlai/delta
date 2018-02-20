#include "parse.h"
#include <forward_list>
#include <sstream>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#pragma warning(pop)
#include "lex.h"
#include "../ast/decl.h"
#include "../ast/module.h"
#include "../ast/token.h"
#include "../support/utility.h"

using namespace delta;

static std::unique_ptr<llvm::MemoryBuffer> getFileMemoryBuffer(llvm::StringRef filePath) {
    auto buffer = llvm::MemoryBuffer::getFile(filePath);
    if (!buffer) printErrorAndExit("couldn't open file '", filePath, "'");
    return std::move(*buffer);
}

Parser::Parser(llvm::StringRef filePath, Module& module, llvm::ArrayRef<std::string> importSearchPaths,
               llvm::ArrayRef<std::string> frameworkSearchPaths)
: Parser(getFileMemoryBuffer(filePath), module, importSearchPaths, frameworkSearchPaths) {}

Parser::Parser(std::unique_ptr<llvm::MemoryBuffer> input, Module& module, llvm::ArrayRef<std::string> importSearchPaths,
               llvm::ArrayRef<std::string> frameworkSearchPaths)
: lexer(std::move(input)), currentModule(&module), currentTokenIndex(0), importSearchPaths(importSearchPaths),
  frameworkSearchPaths(frameworkSearchPaths) {
    tokenBuffer.emplace_back(lexer.nextToken());
}

Token Parser::currentToken() {
    ASSERT(currentTokenIndex < tokenBuffer.size());
    return tokenBuffer[currentTokenIndex];
}

SourceLocation Parser::getCurrentLocation() {
    return currentToken().getLocation();
}

Token Parser::lookAhead(int offset) {
    if (int(currentTokenIndex) + offset < 0) return Token(Token::None, SourceLocation());
    int count = int(currentTokenIndex) + offset - int(tokenBuffer.size()) + 1;
    while (count-- > 0) {
        tokenBuffer.emplace_back(lexer.nextToken());
    }
    return tokenBuffer[currentTokenIndex + offset];
}

Token Parser::consumeToken() {
    Token token = currentToken();
    if (++currentTokenIndex == tokenBuffer.size()) {
        tokenBuffer.emplace_back(lexer.nextToken());
    }
    return token;
}

/// Adds quotes around the string representation of the given token unless
/// it's an identifier, numeric literal, string literal, or end-of-file.
static std::string quote(Token::Kind tokenKind) {
    std::ostringstream stream;
    if (tokenKind < Token::Break) {
        stream << tokenKind;
    } else {
        stream << '\'' << tokenKind << '\'';
    }
    return stream.str();
}

[[noreturn]] static void unexpectedToken(Token token, llvm::ArrayRef<Token::Kind> expected = {},
                                         const char* contextInfo = nullptr) {
    if (expected.size() == 0) {
        error(token.getLocation(), "unexpected ", quote(token), contextInfo ? " " : "", contextInfo ? contextInfo : "");
    } else {
        error(token.getLocation(), "expected ", toDisjunctiveList(expected, quote), contextInfo ? " " : "",
              contextInfo ? contextInfo : "", ", got ", quote(token));
    }
}

void Parser::expect(llvm::ArrayRef<Token::Kind> expected, const char* contextInfo) {
    if (!llvm::is_contained(expected, currentToken())) {
        unexpectedToken(currentToken(), expected, contextInfo);
    }
}

Token Parser::parse(llvm::ArrayRef<Token::Kind> expected, const char* contextInfo) {
    expect(expected, contextInfo);
    return consumeToken();
}

void Parser::checkStmtTerminatorConsistency(Token::Kind currentTerminator, llvm::function_ref<SourceLocation()> getLocation) {
    static Token::Kind previousTerminator = Token::None;
    static const char* filePath = nullptr;

    if (filePath != lexer.getFilePath()) {
        filePath = lexer.getFilePath();
        previousTerminator = Token::None;
    }

    if (previousTerminator == Token::None) {
        previousTerminator = currentTerminator;
    } else if (previousTerminator != currentTerminator) {
        warning(getLocation(), "inconsistent statement terminator, expected ", quote(previousTerminator));
    }
}

void Parser::parseStmtTerminator(const char* contextInfo) {
    if (getCurrentLocation().line != lookAhead(-1).getLocation().line) {
        checkStmtTerminatorConsistency(Token::Newline, [this] {
            // TODO: Use pre-existing buffer instead of reading from file here.
            readLineFromFile(lookAhead(-1).getLocation());
            std::ifstream file(getCurrentLocation().file);
            for (auto line = lookAhead(-1).getLocation().line; --line;) {
                file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            }
            std::string line;
            std::getline(file, line);
            return SourceLocation(getCurrentLocation().file, lookAhead(-1).getLocation().line,
                                  static_cast<SourceLocation::IntegerType>(line.size() + 1));
        });
        return;
    }

    switch (currentToken()) {
        case Token::RightBrace:
            checkStmtTerminatorConsistency(Token::Newline, std::bind(&Parser::getCurrentLocation, this));
            return;
        case Token::Semicolon:
            checkStmtTerminatorConsistency(Token::Semicolon, std::bind(&Parser::getCurrentLocation, this));
            consumeToken();
            return;
        default:
            unexpectedToken(currentToken(), { Token::Newline, Token::Semicolon }, contextInfo);
    }
}

/// argument-list ::= '(' ')' | '(' nonempty-argument-list ')'
/// nonempty-argument-list ::= argument | nonempty-argument-list ',' argument
/// argument ::= (id ':')? expr
std::vector<NamedValue> Parser::parseArgumentList() {
    parse(Token::LeftParen);
    std::vector<NamedValue> args;
    while (currentToken() != Token::RightParen) {
        std::string name;
        SourceLocation location = SourceLocation();
        if (lookAhead(1) == Token::Colon) {
            auto result = parse(Token::Identifier);
            name = std::move(result.getString());
            location = result.getLocation();
            consumeToken();
        }
        auto value = parseExpr();
        if (!location.isValid()) location = value->getLocation();
        args.push_back({ std::move(name), std::move(value), location });
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }
    consumeToken();
    return args;
}

/// var-expr ::= id
std::unique_ptr<VarExpr> Parser::parseVarExpr() {
    ASSERT(currentToken().is(Token::Identifier, Token::Init));
    auto id = consumeToken();
    return llvm::make_unique<VarExpr>(id.getString(), id.getLocation());
}

std::unique_ptr<VarExpr> Parser::parseThis() {
    ASSERT(currentToken() == Token::This);
    auto expr = llvm::make_unique<VarExpr>("this", getCurrentLocation());
    consumeToken();
    return expr;
}

static std::string replaceEscapeChars(llvm::StringRef literalContent, SourceLocation literalStartLocation) {
    std::string result;
    result.reserve(literalContent.size());

    for (auto it = literalContent.begin(), end = literalContent.end(); it != end; ++it) {
        if (*it == '\\') {
            ++it;
            ASSERT(it != end);
            switch (*it) {
                case '0':
                    result += '\0';
                    break;
                case 'a':
                    result += '\a';
                    break;
                case 'b':
                    result += '\b';
                    break;
                case 'n':
                    result += '\n';
                    break;
                case 'r':
                    result += '\r';
                    break;
                case 't':
                    result += '\t';
                    break;
                case 'v':
                    result += '\v';
                    break;
                case '"':
                    result += '"';
                    break;
                case '\'':
                    result += '\'';
                    break;
                case '\\':
                    result += '\\';
                    break;
                default:
                    auto itColumn = literalStartLocation.column + 1 + (it - literalContent.begin());
                    SourceLocation itLocation(literalStartLocation.file, literalStartLocation.line, itColumn);
                    error(itLocation, "unknown escape character '\\", *it, "'");
            }
            continue;
        }
        result += *it;
    }
    return result;
}

std::unique_ptr<StringLiteralExpr> Parser::parseStringLiteral() {
    ASSERT(currentToken() == Token::StringLiteral);
    auto content = replaceEscapeChars(currentToken().getString().drop_back().drop_front(), getCurrentLocation());
    auto expr = llvm::make_unique<StringLiteralExpr>(std::move(content), getCurrentLocation());
    consumeToken();
    return expr;
}

std::unique_ptr<CharacterLiteralExpr> Parser::parseCharacterLiteral() {
    ASSERT(currentToken() == Token::CharacterLiteral);
    auto content = replaceEscapeChars(currentToken().getString().drop_back().drop_front(), getCurrentLocation());
    if (content.size() != 1) error(getCurrentLocation(), "character literal must consist of a single UTF-8 byte");
    auto expr = llvm::make_unique<CharacterLiteralExpr>(content[0], getCurrentLocation());
    consumeToken();
    return expr;
}

std::unique_ptr<IntLiteralExpr> Parser::parseIntLiteral() {
    ASSERT(currentToken() == Token::IntegerLiteral);
    auto expr = llvm::make_unique<IntLiteralExpr>(currentToken().getIntegerValue(), getCurrentLocation());
    consumeToken();
    return expr;
}

std::unique_ptr<FloatLiteralExpr> Parser::parseFloatLiteral() {
    ASSERT(currentToken() == Token::FloatLiteral);
    auto expr = llvm::make_unique<FloatLiteralExpr>(currentToken().getFloatingPointValue(), getCurrentLocation());
    consumeToken();
    return expr;
}

std::unique_ptr<BoolLiteralExpr> Parser::parseBoolLiteral() {
    std::unique_ptr<BoolLiteralExpr> expr;
    switch (currentToken()) {
        case Token::True:
            expr = llvm::make_unique<BoolLiteralExpr>(true, getCurrentLocation());
            break;
        case Token::False:
            expr = llvm::make_unique<BoolLiteralExpr>(false, getCurrentLocation());
            break;
        default:
            llvm_unreachable("all cases handled");
    }
    consumeToken();
    return expr;
}

std::unique_ptr<NullLiteralExpr> Parser::parseNullLiteral() {
    ASSERT(currentToken() == Token::Null);
    auto expr = llvm::make_unique<NullLiteralExpr>(getCurrentLocation());
    consumeToken();
    return expr;
}

std::unique_ptr<UndefinedLiteralExpr> Parser::parseUndefinedLiteral() {
    ASSERT(currentToken() == Token::Undefined);
    auto expr = llvm::make_unique<UndefinedLiteralExpr>(getCurrentLocation());
    consumeToken();
    return expr;
}

/// array-literal ::= '[' expr-list ']'
std::unique_ptr<ArrayLiteralExpr> Parser::parseArrayLiteral() {
    ASSERT(currentToken() == Token::LeftBracket);
    auto location = getCurrentLocation();
    consumeToken();
    auto elements = parseExprList();
    parse(Token::RightBracket);
    return llvm::make_unique<ArrayLiteralExpr>(std::move(elements), location);
}

/// tuple-literal ::= '(' tuple-literal-elements ')'
/// tuple-literal-elements ::= tuple-literal-element | tuple-literal-elements ',' tuple-literal-element
/// tuple-literal-element ::= id ':' expr
std::unique_ptr<TupleExpr> Parser::parseTupleLiteral() {
    ASSERT(currentToken() == Token::LeftParen);
    auto location = getCurrentLocation();
    auto elements = parseArgumentList();
    for (auto& element : elements) {
        if (element.getName().empty()) {
            if (auto* varExpr = llvm::dyn_cast<VarExpr>(element.getValue())) {
                element.setName(varExpr->getIdentifier());
            } else {
                error(element.getLocation(), "tuple elements must have names");
            }
        }
    }
    return llvm::make_unique<TupleExpr>(std::move(elements), location);
}

/// non-empty-type-list ::= type | type ',' non-empty-type-list
std::vector<Type> Parser::parseNonEmptyTypeList() {
    std::vector<Type> types;

    while (true) {
        types.push_back(parseType());

        if (currentToken() == Token::Comma) {
            consumeToken();
            continue;
        }

        if (currentToken() == Token::RightShift) {
            tokenBuffer[currentTokenIndex] = Token(Token::Greater, currentToken().getLocation());
            tokenBuffer.insert(tokenBuffer.begin() + currentTokenIndex + 1,
                               Token(Token::Greater, currentToken().getLocation().nextColumn()));
        }

        break;
    }

    return types;
}

/// generic-argument-list ::= '<' non-empty-type-list '>'
std::vector<Type> Parser::parseGenericArgumentList() {
    ASSERT(currentToken() == Token::Less);
    consumeToken();
    std::vector<Type> genericArgs = parseNonEmptyTypeList();
    parse(Token::Greater);
    return genericArgs;
}

int64_t Parser::parseArraySizeInBrackets() {
    ASSERT(currentToken() == Token::LeftBracket);
    consumeToken();
    int64_t arraySize;

    switch (currentToken()) {
        case Token::IntegerLiteral:
            arraySize = consumeToken().getIntegerValue();
            break;
        case Token::RightBracket:
            arraySize = ArrayType::runtimeSize;
            break;
        case Token::QuestionMark:
            consumeToken();
            arraySize = ArrayType::unknownSize;
            break;
        default:
            error(getCurrentLocation(), "non-literal array bounds not implemented yet");
    }

    parse(Token::RightBracket);
    return arraySize;
}

/// simple-type ::= id | id generic-argument-list | id '[' int-literal? ']'
Type Parser::parseSimpleType(bool isMutable) {
    auto identifier = parse(Token::Identifier);
    std::vector<Type> genericArgs;

    switch (currentToken()) {
        case Token::Less:
            genericArgs = parseGenericArgumentList();
            LLVM_FALLTHROUGH;
        default:
            return BasicType::get(identifier.getString(), std::move(genericArgs), isMutable, identifier.getLocation());
        case Token::LeftBracket:
            auto bracketLocation = getCurrentLocation();
            Type elementType = BasicType::get(identifier.getString(), {}, isMutable, identifier.getLocation());
            return ArrayType::get(elementType, parseArraySizeInBrackets(), false, bracketLocation);
    }
}

/// tuple-type ::= '(' tuple-type-elements ')'
/// tuple-type-elements ::= tuple-type-element | tuple-type-elements ',' tuple-type-element
/// tuple-type-element ::= id ':' type
Type Parser::parseTupleType() {
    ASSERT(currentToken() == Token::LeftParen);
    auto location = getCurrentLocation();
    consumeToken();
    std::vector<TupleElement> elements;

    while (currentToken() != Token::RightParen) {
        std::string name = parse(Token::Identifier).getString();
        parse(Token::Colon);
        elements.push_back({ std::move(name), parseType() });
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }

    consumeToken();
    return TupleType::get(std::move(elements), false, location);
}

/// function-type ::= param-type-list '->' type
/// param-type-list ::= '(' param-types ')'
/// param-types ::= '' | non-empty-param-types
/// non-empty-param-types ::= type | type ',' non-empty-param-types
Type Parser::parseFunctionType() {
    ASSERT(currentToken() == Token::LeftParen);
    auto location = getCurrentLocation();
    consumeToken();
    std::vector<Type> paramTypes;

    while (currentToken() != Token::RightParen) {
        paramTypes.emplace_back(parseType());
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }

    consumeToken();
    parse(Token::RightArrow);
    Type returnType = parseType();
    return FunctionType::get(returnType, std::move(paramTypes), false, location);
}

/// type ::= simple-type | 'mutable' simple-type | type 'mutable'? '*' | type 'mutable'? '?' |
///          function-type | tuple-type
Type Parser::parseType() {
    Type type;
    auto location = getCurrentLocation();

    switch (currentToken()) {
        case Token::Identifier:
            type = parseSimpleType(false);
            break;
        case Token::Mutable:
            consumeToken();
            type = parseSimpleType(true);
            type.setMutable(true);
            break;
        case Token::LeftParen:
            if (arrowAfterParentheses()) {
                type = parseFunctionType();
            } else {
                type = parseTupleType();
            }
            break;
        default:
            unexpectedToken(currentToken(), { Token::Identifier, Token::Mutable, Token::LeftParen });
    }

    while (true) {
        switch (currentToken()) {
            case Token::Star:
                type = PointerType::get(type, false, getCurrentLocation());
                consumeToken();
                break;
            case Token::QuestionMark:
                type = OptionalType::get(type, false, getCurrentLocation());
                consumeToken();
                break;
            case Token::Mutable:
                consumeToken();
                switch (currentToken()) {
                    case Token::Star:
                        type = PointerType::get(type, true, getCurrentLocation());
                        consumeToken();
                        break;
                    case Token::QuestionMark:
                        type = OptionalType::get(type, true, getCurrentLocation());
                        consumeToken();
                        break;
                    default:
                        unexpectedToken(currentToken(), { Token::Star, Token::QuestionMark }, "after 'mutable'");
                }
                break;
            case Token::LeftBracket:
                type = ArrayType::get(type, parseArraySizeInBrackets(), false, getCurrentLocation());
                break;
            case Token::And:
                error(getCurrentLocation(), "Delta doesn't have C++-style references; ",
                      "use pointers ('*') instead, they are non-null by default");
            default:
                return type.withLocation(location);
        }
    }
}

/// cast-expr ::= 'cast' '<' type '>' '(' expr ')'
std::unique_ptr<CastExpr> Parser::parseCastExpr() {
    ASSERT(currentToken() == Token::Cast);
    auto location = getCurrentLocation();
    consumeToken();
    parse(Token::Less);
    auto type = parseType();
    parse(Token::Greater);
    parse(Token::LeftParen);
    auto expr = parseExpr();
    parse(Token::RightParen);
    return llvm::make_unique<CastExpr>(type, std::move(expr), location);
}

/// sizeof-expr ::= 'sizeof' '(' type ')'
std::unique_ptr<SizeofExpr> Parser::parseSizeofExpr() {
    assert(currentToken() == Token::Sizeof);
    auto location = getCurrentLocation();
    consumeToken();
    parse(Token::LeftParen);
    auto type = parseType();
    parse(Token::RightParen);
    return llvm::make_unique<SizeofExpr>(type, location);
}

/// addressof-expr ::= 'addressof' '(' expr ')'
std::unique_ptr<AddressofExpr> Parser::parseAddressofExpr() {
    assert(currentToken() == Token::Addressof);
    auto location = getCurrentLocation();
    consumeToken();
    parse(Token::LeftParen);
    auto operand = parseExpr();
    parse(Token::RightParen);
    return llvm::make_unique<AddressofExpr>(std::move(operand), location);
}

/// member-expr ::= expr '.' id
std::unique_ptr<MemberExpr> Parser::parseMemberExpr(std::unique_ptr<Expr> lhs) {
    auto location = getCurrentLocation();
    llvm::StringRef member;

    if (currentToken().is(Token::Identifier, Token::Init, Token::Deinit)) {
        member = consumeToken().getString();
    } else {
        unexpectedToken(currentToken(), Token::Identifier);
    }

    return llvm::make_unique<MemberExpr>(std::move(lhs), member, location);
}

/// subscript-expr ::= expr '[' expr ']'
std::unique_ptr<SubscriptExpr> Parser::parseSubscript(std::unique_ptr<Expr> operand) {
    ASSERT(currentToken() == Token::LeftBracket);
    auto location = getCurrentLocation();
    consumeToken();
    auto index = parseExpr();
    parse(Token::RightBracket);
    return llvm::make_unique<SubscriptExpr>(std::move(operand), std::move(index), location);
}

/// unwrap-expr ::= expr '!'
std::unique_ptr<UnwrapExpr> Parser::parseUnwrapExpr(std::unique_ptr<Expr> operand) {
    ASSERT(currentToken() == Token::Not);
    auto location = getCurrentLocation();
    consumeToken();
    return llvm::make_unique<UnwrapExpr>(std::move(operand), location);
}

/// call-expr ::= expr generic-argument-list? argument-list
std::unique_ptr<CallExpr> Parser::parseCallExpr(std::unique_ptr<Expr> callee) {
    std::vector<Type> genericArgs;
    if (currentToken() == Token::Less) {
        genericArgs = parseGenericArgumentList();
    }
    auto location = getCurrentLocation();
    auto args = parseArgumentList();
    return llvm::make_unique<CallExpr>(std::move(callee), std::move(args), std::move(genericArgs), location);
}

std::unique_ptr<LambdaExpr> Parser::parseLambdaExpr() {
    ASSERT(currentToken().is(Token::LeftParen, Token::Identifier));
    auto location = getCurrentLocation();
    std::vector<ParamDecl> params;

    if (currentToken() == Token::Identifier) {
        params.push_back(parseParam(true));
    } else {
        params = parseParamList(nullptr, true);
    }

    parse(Token::RightArrow);
    auto body = parseExpr();
    return llvm::make_unique<LambdaExpr>(std::move(params), std::move(body), location);
}

/// paren-expr ::= '(' expr ')'
std::unique_ptr<Expr> Parser::parseParenExpr() {
    ASSERT(currentToken() == Token::LeftParen);
    consumeToken();
    auto expr = parseExpr();
    parse(Token::RightParen);
    return expr;
}

/// if-expr ::= expr '?' expr ':' expr
std::unique_ptr<IfExpr> Parser::parseIfExpr(std::unique_ptr<Expr> condition) {
    ASSERT(currentToken() == Token::QuestionMark);
    auto location = getCurrentLocation();
    consumeToken();
    auto thenExpr = parseExpr();
    parse(Token::Colon);
    auto elseExpr = parseExpr();
    return llvm::make_unique<IfExpr>(std::move(condition), std::move(thenExpr), std::move(elseExpr), location);
}

bool Parser::shouldParseGenericArgumentList() {
    // Temporary hack: use spacing to determine whether to parse a generic argument list
    // of a less-than binary expression. Zero spaces on either side of '<' will cause it
    // to be interpreted as a generic argument list, for now.
    return lookAhead(0).getLocation().column + int(lookAhead(0).getString().size()) == lookAhead(1).getLocation().column ||
           lookAhead(1).getLocation().column + 1 == lookAhead(2).getLocation().column;
}

/// Returns true if a right-arrow token immediately follows the current set of parentheses.
bool Parser::arrowAfterParentheses() {
    ASSERT(currentToken() == Token::LeftParen);
    int offset = 1;

    for (int parenDepth = 1; parenDepth > 0; ++offset) {
        switch (lookAhead(offset)) {
            case Token::LeftParen:
                ++parenDepth;
                break;
            case Token::RightParen:
                --parenDepth;
                break;
            default:
                break;
        }
    }

    return lookAhead(offset) == Token::RightArrow;
}

/// postfix-expr ::= postfix-expr postfix-op | call-expr | variable-expr | string-literal |
///                  int-literal | float-literal | bool-literal | null-literal |
///                  paren-expr | array-literal | tuple-literal | cast-expr | subscript-expr |
///                  member-expr | unwrap-expr | lambda-expr | sizeof-expr | addressof-expr
std::unique_ptr<Expr> Parser::parsePostfixExpr() {
    std::unique_ptr<Expr> expr;
    switch (currentToken()) {
        case Token::Identifier:
        case Token::Init:
            switch (lookAhead(1)) {
                case Token::LeftParen:
                    expr = parseCallExpr(parseVarExpr());
                    break;
                case Token::Less:
                    if (shouldParseGenericArgumentList()) {
                        expr = parseCallExpr(parseVarExpr());
                        break;
                    }
                    LLVM_FALLTHROUGH;
                default:
                    expr = parseVarExpr();
                    break;
            }
            break;
        case Token::StringLiteral:
            expr = parseStringLiteral();
            break;
        case Token::CharacterLiteral:
            expr = parseCharacterLiteral();
            break;
        case Token::IntegerLiteral:
            expr = parseIntLiteral();
            break;
        case Token::FloatLiteral:
            expr = parseFloatLiteral();
            break;
        case Token::True:
        case Token::False:
            expr = parseBoolLiteral();
            break;
        case Token::Null:
            expr = parseNullLiteral();
            break;
        case Token::This:
            expr = parseThis();
            break;
        case Token::LeftParen:
            if (lookAhead(1) == Token::RightParen && lookAhead(2) == Token::RightArrow) {
                expr = parseLambdaExpr();
            } else if (lookAhead(1) == Token::Identifier && lookAhead(2).is(Token::Colon, Token::Comma)) {
                if (arrowAfterParentheses()) {
                    expr = parseLambdaExpr();
                } else {
                    expr = parseTupleLiteral();
                }
            } else {
                expr = parseParenExpr();
            }
            break;
        case Token::LeftBracket:
            expr = parseArrayLiteral();
            break;
        case Token::Cast:
            expr = parseCastExpr();
            break;
        case Token::Sizeof:
            expr = parseSizeofExpr();
            break;
        case Token::Addressof:
            expr = parseAddressofExpr();
            break;
        case Token::Undefined:
            expr = parseUndefinedLiteral();
            break;
        default:
            unexpectedToken(currentToken());
            break;
    }
    while (true) {
        switch (currentToken()) {
            case Token::LeftBracket:
                expr = parseSubscript(std::move(expr));
                break;
            case Token::LeftParen:
                expr = parseCallExpr(std::move(expr));
                break;
            case Token::Dot:
                consumeToken();
                expr = parseMemberExpr(std::move(expr));
                break;
            case Token::Increment:
            case Token::Decrement:
                expr = parseIncrementOrDecrementExpr(std::move(expr));
                break;
            case Token::Not:
                expr = parseUnwrapExpr(std::move(expr));
                break;
            case Token::QuestionMark:
                expr = parseIfExpr(std::move(expr));
                break;
            default:
                return expr;
        }
    }
}

/// prefix-expr ::= prefix-operator (prefix-expr | postfix-expr)
std::unique_ptr<UnaryExpr> Parser::parsePrefixExpr() {
    ASSERT(isUnaryOperator(currentToken()));
    auto op = currentToken();
    auto location = getCurrentLocation();
    consumeToken();
    return llvm::make_unique<UnaryExpr>(op, parsePreOrPostfixExpr(), location);
}

std::unique_ptr<Expr> Parser::parsePreOrPostfixExpr() {
    return isUnaryOperator(currentToken()) ? parsePrefixExpr() : parsePostfixExpr();
}

/// inc-expr ::= expr '++'
/// dec-expr ::= expr '--'
std::unique_ptr<UnaryExpr> Parser::parseIncrementOrDecrementExpr(std::unique_ptr<Expr> operand) {
    auto location = getCurrentLocation();
    return llvm::make_unique<UnaryExpr>(parse({ Token::Increment, Token::Decrement }), std::move(operand), location);
}

/// binary-expr ::= expr op expr
std::unique_ptr<Expr> Parser::parseBinaryExpr(std::unique_ptr<Expr> lhs, int minPrecedence) {
    while (isBinaryOperator(currentToken()) && getPrecedence(currentToken()) >= minPrecedence) {
        auto backtrackLocation = currentTokenIndex;
        auto op = consumeToken();
        auto expr = parsePreOrPostfixExpr();
        if (isAssignmentOperator(currentToken())) {
            currentTokenIndex = backtrackLocation;
            break;
        }

        while (isBinaryOperator(currentToken()) && getPrecedence(currentToken()) > getPrecedence(op)) {
            auto token = consumeToken();
            expr = llvm::make_unique<BinaryExpr>(token, std::move(expr), parsePreOrPostfixExpr(), token.getLocation());
        }
        lhs = llvm::make_unique<BinaryExpr>(op, std::move(lhs), std::move(expr), op.getLocation());
    }
    return lhs;
}

/// expr ::= prefix-expr | postfix-expr | binary-expr
std::unique_ptr<Expr> Parser::parseExpr() {
    return parseBinaryExpr(parsePreOrPostfixExpr(), 0);
}

/// expr-list ::= '' | nonempty-expr-list
/// nonempty-expr-list ::= expr | expr ',' nonempty-expr-list
std::vector<std::unique_ptr<Expr>> Parser::parseExprList() {
    std::vector<std::unique_ptr<Expr>> exprs;

    // TODO: Handle empty expression list.
    if (currentToken() == Token::Semicolon || currentToken() == Token::RightBrace) {
        return exprs;
    }

    while (true) {
        exprs.emplace_back(parseExpr());
        if (currentToken() != Token::Comma) return exprs;
        consumeToken();
    }
}

/// return-stmt ::= 'return' expr ('\n' | ';')
std::unique_ptr<ReturnStmt> Parser::parseReturnStmt() {
    ASSERT(currentToken() == Token::Return);
    auto location = getCurrentLocation();
    consumeToken();

    std::unique_ptr<Expr> returnValue;
    if (!currentToken().is(Token::Semicolon, Token::RightBrace)) {
        returnValue = parseExpr();
    }

    parseStmtTerminator();
    return llvm::make_unique<ReturnStmt>(std::move(returnValue), location);
}

/// var-decl ::= mutability-specifier id type-specifier? '=' initializer ('\n' | ';')
/// mutability-specifier ::= 'let' | 'var'
/// type-specifier ::= ':' type
/// initializer ::= expr | 'undefined'
std::unique_ptr<VarDecl> Parser::parseVarDecl(bool requireInitialValue, Decl* parent, AccessLevel accessLevel) {
    bool isMutable = parse({ Token::Let, Token::Var }) == Token::Var;
    auto name = parse(Token::Identifier);

    Type type;
    if (currentToken() == Token::Colon) {
        consumeToken();
        auto typeLocation = getCurrentLocation();
        type = parseType();
        if (type.isMutable()) error(typeLocation, "type specifier cannot specify mutability");
    }
    type.setMutable(isMutable);

    std::unique_ptr<Expr> initializer;

    if (requireInitialValue) {
        parse(Token::Assignment);
        initializer = parseExpr();
        parseStmtTerminator();
    }

    return llvm::make_unique<VarDecl>(type, name.getString(), std::move(initializer), parent, accessLevel,
                                      *currentModule, name.getLocation());
}

/// var-stmt ::= var-decl
std::unique_ptr<VarStmt> Parser::parseVarStmt(Decl* parent) {
    return llvm::make_unique<VarStmt>(parseVarDecl(true, parent, AccessLevel::None));
}

/// expr-stmt ::= binary-expr | call-expr ('\n' | ';')
std::unique_ptr<ExprStmt> Parser::parseExprStmt(std::unique_ptr<Expr> expr) {
    auto stmt = llvm::make_unique<ExprStmt>(std::move(expr));
    parseStmtTerminator();
    return stmt;
}

/// defer-stmt ::= 'defer' call-expr ('\n' | ';')
std::unique_ptr<DeferStmt> Parser::parseDeferStmt() {
    ASSERT(currentToken() == Token::Defer);
    consumeToken();
    // FIXME: Doesn't have to be a variable expression.
    auto stmt = llvm::make_unique<DeferStmt>(parseCallExpr(parseVarExpr()));
    parseStmtTerminator();
    return stmt;
}

/// if-stmt ::= 'if' '(' expr ')' '{' stmt* '}' ('else' else-branch)?
/// else-branch ::= if-stmt | '{' stmt* '}'
std::unique_ptr<IfStmt> Parser::parseIfStmt(Decl* parent) {
    ASSERT(currentToken() == Token::If);
    consumeToken();
    parse(Token::LeftParen);
    auto condition = parseExpr();
    parse(Token::RightParen);
    parse(Token::LeftBrace);
    auto thenStmts = parseStmtsUntil(Token::RightBrace, parent);
    parse(Token::RightBrace);
    std::vector<std::unique_ptr<Stmt>> elseStmts;
    if (currentToken() != Token::Else) {
        return llvm::make_unique<IfStmt>(std::move(condition), std::move(thenStmts), std::move(elseStmts));
    }
    consumeToken();
    if (currentToken() == Token::LeftBrace) {
        consumeToken();
        elseStmts = parseStmtsUntil(Token::RightBrace, parent);
        parse(Token::RightBrace);
        return llvm::make_unique<IfStmt>(std::move(condition), std::move(thenStmts), std::move(elseStmts));
    }
    if (currentToken() == Token::If) {
        elseStmts.emplace_back(parseIfStmt(parent));
        return llvm::make_unique<IfStmt>(std::move(condition), std::move(thenStmts), std::move(elseStmts));
    }
    unexpectedToken(currentToken(), { Token::LeftBrace, Token::If });
}

/// while-stmt ::= 'while' '(' expr ')' '{' stmt* '}'
std::unique_ptr<WhileStmt> Parser::parseWhileStmt(Decl* parent) {
    ASSERT(currentToken() == Token::While);
    consumeToken();
    parse(Token::LeftParen);
    auto condition = parseExpr();
    parse(Token::RightParen);
    parse(Token::LeftBrace);
    auto body = parseStmtsUntil(Token::RightBrace, parent);
    parse(Token::RightBrace);
    return llvm::make_unique<WhileStmt>(std::move(condition), std::move(body));
}

/// for-stmt ::= 'for' '(' id 'in' expr ')' '{' stmt* '}'
std::unique_ptr<ForStmt> Parser::parseForStmt(Decl* parent) {
    ASSERT(currentToken() == Token::For);
    auto location = getCurrentLocation();
    consumeToken();
    parse(Token::LeftParen);
    auto variable = parseVarDecl(false, parent, AccessLevel::None);
    parse(Token::In);
    auto range = parseExpr();
    parse(Token::RightParen);
    parse(Token::LeftBrace);
    auto body = parseStmtsUntil(Token::RightBrace, parent);
    parse(Token::RightBrace);
    return llvm::make_unique<ForStmt>(std::move(variable), std::move(range), std::move(body), location);
}

/// switch-stmt ::= 'switch' '(' expr ')' '{' cases default-case? '}'
/// cases ::= case | case cases
/// case ::= 'case' expr ':' stmt+
/// default-case ::= 'default' ':' stmt+
std::unique_ptr<SwitchStmt> Parser::parseSwitchStmt(Decl* parent) {
    ASSERT(currentToken() == Token::Switch);
    consumeToken();
    parse(Token::LeftParen);
    auto condition = parseExpr();
    parse(Token::RightParen);
    parse(Token::LeftBrace);
    std::vector<SwitchCase> cases;
    std::vector<std::unique_ptr<Stmt>> defaultStmts;
    bool defaultSeen = false;
    while (true) {
        if (currentToken() == Token::Case) {
            consumeToken();
            auto value = parseExpr();
            parse(Token::Colon);
            auto stmts = parseStmtsUntilOneOf(Token::Case, Token::Default, Token::RightBrace, parent);
            cases.push_back({ std::move(value), std::move(stmts) });
        } else if (currentToken() == Token::Default) {
            if (defaultSeen) {
                error(getCurrentLocation(), "switch-statement may only contain one 'default' case");
            }
            consumeToken();
            parse(Token::Colon);
            defaultStmts = parseStmtsUntilOneOf(Token::Case, Token::Default, Token::RightBrace, parent);
            defaultSeen = true;
        } else {
            error(getCurrentLocation(), "expected 'case' or 'default'");
        }
        if (currentToken() == Token::RightBrace) break;
    }
    consumeToken();
    return llvm::make_unique<SwitchStmt>(std::move(condition), std::move(cases), std::move(defaultStmts));
}

/// break-stmt ::= 'break' ('\n' | ';')
std::unique_ptr<BreakStmt> Parser::parseBreakStmt() {
    auto location = getCurrentLocation();
    consumeToken();
    parseStmtTerminator();
    return llvm::make_unique<BreakStmt>(location);
}

/// continue-stmt ::= 'continue' ('\n' | ';')
std::unique_ptr<ContinueStmt> Parser::parseContinueStmt() {
    auto location = getCurrentLocation();
    consumeToken();
    parseStmtTerminator();
    return llvm::make_unique<ContinueStmt>(location);
}

/// stmt ::= var-stmt | return-stmt | expr-stmt | defer-stmt | if-stmt |
///          switch-stmt | while-stmt | for-stmt | break-stmt | continue-stmt
std::unique_ptr<Stmt> Parser::parseStmt(Decl* parent) {
    switch (currentToken()) {
        case Token::Return:
            return parseReturnStmt();
        case Token::Let:
        case Token::Var:
            return parseVarStmt(parent);
        case Token::Defer:
            return parseDeferStmt();
        case Token::If:
            return parseIfStmt(parent);
        case Token::While:
            return parseWhileStmt(parent);
        case Token::For:
            return parseForStmt(parent);
        case Token::Switch:
            return parseSwitchStmt(parent);
        case Token::Break:
            return parseBreakStmt();
        case Token::Continue:
            return parseContinueStmt();
        case Token::Underscore: {
            consumeToken();
            parse(Token::Assignment);
            auto stmt = llvm::make_unique<ExprStmt>(parseExpr());
            parseStmtTerminator();
            return std::move(stmt);
        }
        default:
            break;
    }

    // If we're here, the statement starts with an expression.
    std::unique_ptr<Expr> expr = parseExpr();

    if (expr->isCallExpr() || expr->isIncrementOrDecrementExpr() || expr->isAssignment()) {
        return parseExprStmt(std::move(expr));
    } else {
        unexpectedToken(currentToken());
    }
}

std::vector<std::unique_ptr<Stmt>> Parser::parseStmtsUntil(Token::Kind end, Decl* parent) {
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (currentToken() != end) {
        stmts.emplace_back(parseStmt(parent));
    }
    return stmts;
}

std::vector<std::unique_ptr<Stmt>> Parser::parseStmtsUntilOneOf(Token::Kind end1, Token::Kind end2, Token::Kind end3,
                                                                Decl* parent) {
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (currentToken() != end1 && currentToken() != end2 && currentToken() != end3) {
        stmts.emplace_back(parseStmt(parent));
    }
    return stmts;
}

/// param-decl ::= id ':' type
ParamDecl Parser::parseParam(bool withType) {
    auto name = parse(Token::Identifier);
    Type type;

    if (withType) {
        parse(Token::Colon);
        type = parseType();
    }

    return ParamDecl(std::move(type), std::move(name.getString()), name.getLocation());
}

/// param-list ::= '(' params ')'
/// params ::= '' | non-empty-params
/// non-empty-params ::= param-decl | param-decl ',' non-empty-params
std::vector<ParamDecl> Parser::parseParamList(bool* isVariadic, bool withTypes) {
    parse(Token::LeftParen);
    std::vector<ParamDecl> params;
    while (currentToken() != Token::RightParen) {
        if (isVariadic && currentToken() == Token::DotDotDot) {
            consumeToken();
            *isVariadic = true;
            break;
        }
        params.emplace_back(parseParam(withTypes));
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }
    parse(Token::RightParen);
    return params;
}

void Parser::parseGenericParamList(std::vector<GenericParamDecl>& genericParams) {
    parse(Token::Less);
    while (true) {
        auto genericParamName = parse(Token::Identifier);
        genericParams.emplace_back(genericParamName.getString(), genericParamName.getLocation());

        if (currentToken() == Token::Colon) { // Generic type constraint.
            consumeToken();
            auto identifier = parse(Token::Identifier);
            genericParams.back().addConstraint(BasicType::get(identifier.getString(), {}, false, identifier.getLocation()));
            // TODO: Add support for multiple generic type constraints.
        }

        if (currentToken() == Token::Greater) break;
        parse(Token::Comma);
    }
    parse(Token::Greater);
}

/// function-proto ::= 'func' id param-list ('->' type)?
std::unique_ptr<FunctionDecl> Parser::parseFunctionProto(bool isExtern, TypeDecl* receiverTypeDecl, AccessLevel accessLevel,
                                                         std::vector<GenericParamDecl>* genericParams) {
    ASSERT(currentToken() == Token::Func);
    consumeToken();

    bool isValidFunctionName = currentToken() == Token::Identifier || isOverloadable(currentToken()) ||
                               (currentToken() == Token::LeftBracket && lookAhead(1) == Token::RightBracket);
    if (!isValidFunctionName) unexpectedToken(currentToken(), {}, "as function name");

    SourceLocation nameLocation = getCurrentLocation();
    llvm::StringRef name;
    if (currentToken() == Token::Identifier) {
        name = consumeToken().getString();
    } else if (currentToken() == Token::LeftBracket) {
        consumeToken();
        parse(Token::RightBracket);
        name = "[]";
    } else if (receiverTypeDecl) {
        error(nameLocation, "operator functions other than subscript must be non-member functions");
    } else {
        name = toString(consumeToken().getKind());
    }

    if (currentToken() == Token::Less) {
        parseGenericParamList(*genericParams);
    }

    bool isVariadic = false;
    auto params = parseParamList(isExtern ? &isVariadic : nullptr, true);

    Type returnType = Type::getVoid();
    if (currentToken() == Token::RightArrow) {
        consumeToken();
        returnType = parseType();
    }

    FunctionProto proto(std::move(name), std::move(params), std::move(returnType), isVariadic, isExtern);

    if (receiverTypeDecl) {
        return llvm::make_unique<MethodDecl>(std::move(proto), *receiverTypeDecl, std::vector<Type>(), accessLevel,
                                             nameLocation);
    } else {
        return llvm::make_unique<FunctionDecl>(std::move(proto), std::vector<Type>(), accessLevel, *currentModule,
                                               nameLocation);
    }
}

/// function-template-proto ::= 'func' id template-param-list param-list ('->' type)?
/// template-param-list ::= '<' template-param-decls '>'
/// template-param-decls ::= id | id ',' template-param-decls
std::unique_ptr<FunctionTemplate> Parser::parseFunctionTemplateProto(TypeDecl* receiverTypeDecl, AccessLevel accessLevel) {
    std::vector<GenericParamDecl> genericParams;
    auto decl = parseFunctionProto(false, receiverTypeDecl, accessLevel, &genericParams);
    return llvm::make_unique<FunctionTemplate>(std::move(genericParams), std::move(decl), accessLevel);
}

/// function-decl ::= function-proto '{' stmt* '}'
std::unique_ptr<FunctionDecl> Parser::parseFunctionDecl(TypeDecl* receiverTypeDecl, AccessLevel accessLevel,
                                                        bool requireBody) {
    auto decl = parseFunctionProto(false, receiverTypeDecl, accessLevel, nullptr);

    if (requireBody || currentToken() == Token::LeftBrace) {
        parse(Token::LeftBrace);
        decl->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
        parse(Token::RightBrace);
    }

    if (lookAhead(-1) != Token::RightBrace) {
        parseStmtTerminator();
    }

    return decl;
}

/// function-template-decl ::= function-template-proto '{' stmt* '}'
std::unique_ptr<FunctionTemplate> Parser::parseFunctionTemplate(TypeDecl* receiverTypeDecl, AccessLevel accessLevel) {
    auto decl = parseFunctionTemplateProto(receiverTypeDecl, accessLevel);
    parse(Token::LeftBrace);
    decl->getFunctionDecl()->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
    parse(Token::RightBrace);
    return decl;
}

/// extern-function-decl ::= 'extern' function-proto ('\n' | ';')
std::unique_ptr<FunctionDecl> Parser::parseExternFunctionDecl() {
    ASSERT(currentToken() == Token::Extern);
    consumeToken();
    auto decl = parseFunctionProto(true, nullptr, AccessLevel::Default, nullptr);
    parseStmtTerminator();
    return decl;
}

/// init-decl ::= 'init' param-list '{' stmt* '}'
std::unique_ptr<InitDecl> Parser::parseInitDecl(TypeDecl& receiverTypeDecl, AccessLevel accessLevel) {
    auto initLocation = parse(Token::Init).getLocation();
    auto params = parseParamList(nullptr, true);
    auto decl = llvm::make_unique<InitDecl>(receiverTypeDecl, std::move(params), accessLevel, initLocation);
    parse(Token::LeftBrace);
    decl->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
    parse(Token::RightBrace);
    return decl;
}

/// deinit-decl ::= 'deinit' '(' ')' '{' stmt* '}'
std::unique_ptr<DeinitDecl> Parser::parseDeinitDecl(TypeDecl& receiverTypeDecl) {
    auto deinitLocation = parse(Token::Deinit).getLocation();
    parse(Token::LeftParen);
    auto expectedRParenLocation = getCurrentLocation();
    if (consumeToken() != Token::RightParen) error(expectedRParenLocation, "deinitializers cannot have parameters");
    auto decl = llvm::make_unique<DeinitDecl>(receiverTypeDecl, deinitLocation);
    parse(Token::LeftBrace);
    decl->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
    parse(Token::RightBrace);
    return decl;
}

/// field-decl ::= ('let' | 'var') id ':' type ('\n' | ';')
FieldDecl Parser::parseFieldDecl(TypeDecl& typeDecl, AccessLevel accessLevel) {
    expect({ Token::Let, Token::Var }, "in field declaration");
    bool isMutable = consumeToken() == Token::Var;
    auto name = parse(Token::Identifier);

    parse(Token::Colon);
    auto typeLocation = getCurrentLocation();
    Type type = parseType();
    if (type.isMutable()) error(typeLocation, "type specifier cannot specify mutability");
    type.setMutable(isMutable);

    parseStmtTerminator();
    return FieldDecl(type, std::move(name.getString()), typeDecl, accessLevel, name.getLocation());
}

/// type-template-decl ::= ('struct' | 'interface') id generic-param-list? '{' member-decl* '}'
std::unique_ptr<TypeTemplate> Parser::parseTypeTemplate(AccessLevel accessLevel) {
    std::vector<GenericParamDecl> genericParams;
    auto typeDecl = parseTypeDecl(&genericParams, accessLevel);
    return llvm::make_unique<TypeTemplate>(std::move(genericParams), std::move(typeDecl), accessLevel);
}

Token Parser::parseTypeHeader(std::vector<Type>& interfaces, std::vector<GenericParamDecl>* genericParams) {
    auto name = parse(Token::Identifier);

    if (currentToken() == Token::Less) {
        parseGenericParamList(*genericParams);
    }

    if (currentToken() == Token::Colon) {
        consumeToken();
        interfaces = parseNonEmptyTypeList();
    }

    return name;
}

/// type-decl ::= ('struct' | 'interface') id generic-param-list? interface-list? '{' member-decl* '}'
/// interface-list ::= ':' non-empty-type-list
/// member-decl ::= field-decl | function-decl
std::unique_ptr<TypeDecl> Parser::parseTypeDecl(std::vector<GenericParamDecl>* genericParams, AccessLevel typeAccessLevel) {
    TypeTag tag;
    switch (consumeToken()) {
        case Token::Struct:
            tag = TypeTag::Struct;
            break;
        case Token::Interface:
            tag = TypeTag::Interface;
            break;
        default:
            llvm_unreachable("invalid token");
    }

    std::vector<Type> interfaces;
    auto name = parseTypeHeader(interfaces, genericParams);

    auto typeDecl = llvm::make_unique<TypeDecl>(tag, name.getString(), std::vector<Type>(), std::move(interfaces),
                                                typeAccessLevel, *currentModule, name.getLocation());
    parse(Token::LeftBrace);

    while (currentToken() != Token::RightBrace) {
        AccessLevel accessLevel = AccessLevel::Default;
        bool isMutating = false;

    start:
        switch (currentToken()) {
            case Token::Mutating:
                if (isMutating) {
                    warning(getCurrentLocation(), "duplicate 'mutating' specifier");
                }
                isMutating = true;
                consumeToken();
                goto start;
            case Token::Private:
                if (tag == TypeTag::Interface) {
                    warning(getCurrentLocation(), "interface members cannot be private");
                }
                if (accessLevel != AccessLevel::Default) {
                    warning(getCurrentLocation(), "duplicate access specifier");
                }
                accessLevel = AccessLevel::Private;
                consumeToken();
                goto start;
            case Token::Func: {
                auto requireBody = tag != TypeTag::Interface;

                if (lookAhead(2) == Token::Less) {
                    auto decl = parseFunctionTemplate(typeDecl.get(), accessLevel);
                    llvm::cast<MethodDecl>(decl->getFunctionDecl())->setMutating(isMutating);
                    typeDecl->addMethod(std::move(decl));
                } else {
                    auto decl = llvm::cast<MethodDecl>(parseFunctionDecl(typeDecl.get(), accessLevel, requireBody));
                    decl->setMutating(isMutating);
                    typeDecl->addMethod(std::move(decl));
                }
                break;
            }
            case Token::Init:
                typeDecl->addMethod(parseInitDecl(*typeDecl, accessLevel));
                break;
            case Token::Deinit:
                if (accessLevel != AccessLevel::Default) {
                    warning(lookAhead(-1).getLocation(), "deinitializers cannot be ", accessLevel);
                }
                typeDecl->addMethod(parseDeinitDecl(*typeDecl));
                break;
            case Token::Let:
            case Token::Var:
                typeDecl->addField(parseFieldDecl(*typeDecl, accessLevel));
                break;
            default:
                unexpectedToken(currentToken());
        }
    }

    consumeToken();
    return typeDecl;
}

/// enum-decl ::= 'enum' id generic-param-list? interface-list? '{' enum-case-decl* '}'
/// enum-case-decl ::= 'case' id ('\n' | ';')
std::unique_ptr<EnumDecl> Parser::parseEnumDecl(std::vector<GenericParamDecl>* genericParams, AccessLevel typeAccessLevel) {
    ASSERT(currentToken() == Token::Enum);
    consumeToken();

    std::vector<Type> interfaces;
    auto name = parseTypeHeader(interfaces, genericParams);

    parse(Token::LeftBrace);
    std::vector<EnumCase> cases;
    auto valueCounter = llvm::APSInt::get(0);

    while (currentToken() != Token::RightBrace) {
        parse(Token::Case);
        auto caseName = parse(Token::Identifier);
        Type associatedType;

        if (currentToken() == Token::LeftParen) {
            consumeToken();
            associatedType = parseType();
            parse(Token::RightParen);
        }

        auto value = llvm::make_unique<IntLiteralExpr>(valueCounter.getExtValue(), caseName.getLocation());
        cases.emplace_back(caseName.getString(), std::move(value), associatedType, caseName.getLocation());
        parseStmtTerminator("after enum case");
        ++valueCounter;
    }

    consumeToken();
    return llvm::make_unique<EnumDecl>(name.getString(), std::move(cases), typeAccessLevel, *currentModule,
                                       name.getLocation());
}

/// import-decl ::= 'import' (id | string-literal) ('\n' | ';')
std::unique_ptr<ImportDecl> Parser::parseImportDecl() {
    ASSERT(currentToken() == Token::Import);
    consumeToken();

    std::string importTarget;
    auto location = getCurrentLocation();

    if (currentToken() == Token::StringLiteral) {
        importTarget = parseStringLiteral()->getValue();
    } else {
        importTarget = parse({ Token::Identifier, Token::StringLiteral }, "after 'import'").getString();
    }

    parseStmtTerminator("after 'import' declaration");
    return llvm::make_unique<ImportDecl>(std::move(importTarget), *currentModule, location);
}

void Parser::parseIfdefBody(std::vector<std::unique_ptr<Decl>>* activeDecls) {
    if (currentToken() == Token::HashIf) {
        parseIfdef(activeDecls);
    } else {
        if (activeDecls) {
            activeDecls->emplace_back(parseTopLevelDecl(true));
        } else {
            parseTopLevelDecl(false);
        }
    }
}

void Parser::parseIfdef(std::vector<std::unique_ptr<Decl>>* activeDecls) {
    ASSERT(currentToken() == Token::HashIf);
    consumeToken();
    bool negate = currentToken() == Token::Not;
    if (negate) consumeToken();
    auto identifier = parse(Token::Identifier);

    bool condition = false;
    if (identifier.getString() == "hasInclude") {
        parse(Token::LeftParen);
        auto header = parse(Token::StringLiteral);
        parse(Token::RightParen);

        for (auto& path : llvm::concat<const std::string>(importSearchPaths, frameworkSearchPaths)) {
            auto headerPath = llvm::Twine(path) + "/" + header.getString().drop_back().drop_front();
            if (llvm::sys::fs::exists(headerPath) && !llvm::sys::fs::is_directory(headerPath)) {
                condition = true;
                break;
            }
        }
    } else {
        condition = currentModule->isDefined(identifier.getString());
    }

    if (negate) condition = !condition;

    while (!currentToken().is(Token::HashElse, Token::HashEndif)) {
        parseIfdefBody(condition ? activeDecls : nullptr);
    }

    if (currentToken() == Token::HashElse) {
        consumeToken();
        while (currentToken() != Token::HashEndif) {
            parseIfdefBody(condition ? nullptr : activeDecls);
        }
    }

    consumeToken();
}

/// top-level-decl ::= function-decl | extern-function-decl | type-decl | enum-decl | import-decl | var-decl
std::unique_ptr<Decl> Parser::parseTopLevelDecl(bool addToSymbolTable) {
    AccessLevel accessLevel = AccessLevel::Default;
    std::unique_ptr<Decl> decl;

start:
    switch (currentToken()) {
        case Token::Private:
            if (accessLevel != AccessLevel::Default) warning(getCurrentLocation(), "duplicate access specifier");
            accessLevel = AccessLevel::Private;
            consumeToken();
            goto start;
        case Token::Func:
            if (lookAhead(2) == Token::Less) {
                decl = parseFunctionTemplate(nullptr, accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<FunctionTemplate>(*decl));
            } else {
                decl = parseFunctionDecl(nullptr, accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<FunctionDecl>(*decl));
            }
            break;
        case Token::Extern:
            if (accessLevel != AccessLevel::Default) {
                warning(lookAhead(-1).getLocation(), "extern functions cannot have access specifiers");
            }
            decl = parseExternFunctionDecl();
            if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<FunctionDecl>(*decl));
            break;
        case Token::Struct:
        case Token::Interface:
            if (lookAhead(2) == Token::Less) {
                decl = parseTypeTemplate(accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<TypeTemplate>(*decl));
            } else {
                decl = parseTypeDecl(nullptr, accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<TypeDecl>(*decl));
            }
            break;
        case Token::Enum:
            if (lookAhead(2) == Token::Less) {
                error(getCurrentLocation(), "generic enums not implemented yet");
            } else {
                decl = parseEnumDecl(nullptr, accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<EnumDecl>(*decl));
            }
            break;
        case Token::Let:
        case Token::Var:
            decl = parseVarDecl(true, nullptr, accessLevel);
            if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<VarDecl>(*decl), true);
            break;
        case Token::Import:
            if (accessLevel != AccessLevel::Default) {
                warning(lookAhead(-1).getLocation(), "imports cannot have access specifiers");
            }
            return parseImportDecl();
        default:
            unexpectedToken(currentToken());
    }

    return decl;
}

void Parser::parse() {
    std::vector<std::unique_ptr<Decl>> topLevelDecls;
    SourceFile sourceFile(lexer.getFilePath());

    while (currentToken() != Token::None) {
        if (currentToken() == Token::HashIf) {
            parseIfdef(&topLevelDecls);
        } else {
            topLevelDecls.emplace_back(parseTopLevelDecl(true));
        }
    }

    sourceFile.setDecls(std::move(topLevelDecls));
    currentModule->addSourceFile(std::move(sourceFile));
}
