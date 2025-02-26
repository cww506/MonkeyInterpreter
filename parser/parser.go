package parser

import (
	"fmt"
	"monkey-interpreter/ast"
	"monkey-interpreter/lexer"
	"monkey-interpreter/token"
	"strconv"
)

/*
Here we use iota to give the following constants incrementing numbers as values. The blank identifier _ takes the zero value and the following constants get assigned the values 1 to 7. Which numbers we use doesn’t matter, but the order and the relation to each other do. What we want out of these constants is to later be able to answer: “does the * operator have a higher precedence than the == operator? Does a prefix operator have a higher precedence than a call expression?”
*/
const (
	_ int = iota
	LOWEST
	EQUALS // ==
	LESSGREATER // < or >
	SUM // +
	PRODUCT // *
	// SUFFIX // X-- or X++
	PREFIX // -X or !X
	CALL // myFunction(X)
)

type Parser struct {
	l *lexer.Lexer
	curToken token.Token
	peekToken token.Token
	errors []string
	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns map[token.TokenType]infixParseFn
}

func NewParser(l *lexer.Lexer) *Parser{
	p := &Parser{
		l:l, 
		errors: []string{},
	}

	// Read two tokens so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)

	return p
}

func (p *Parser) Errors() []string{
	return p.errors
}

func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf(`expected next token to be %s, got %s`, t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}


func (p *Parser) nextToken(){
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	// create root node
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	literal := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	literal.Value = value
	return literal
}

func (p *Parser) parseIdentifier() ast.Expression{
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseExpression(precedence int) ast.Expression{
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil{
		return nil
	}
	leftExp := prefix()

	return leftExp
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}
	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON){
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()
	for !p.curTokenIs(token.SEMICOLON){
		p.nextToken()
	}
	return stmt
}

//
func (p *Parser) parseLetStatement() *ast.LetStatement {
	// root node LET
	 stmt := &ast.LetStatement{Token: p.curToken}
	 if !p.expectPeek(token.IDENT){
		return nil
	 }
	// child node IDENT
	 stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	 if !p.expectPeek(token.ASSIGN){
		return nil
	 }
	 // child node 

	 // TODO: we are skipping the expressions until we encounter a semicolon
	 // semicolon == end of stmt
	 for !p.curTokenIs(token.SEMICOLON){
		p.nextToken()
	 }

	 return stmt
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool{
	return p.peekToken.Type == t
}

/* 
Instead of dissecting these tiny methods, let’s talk about expectPeek. The expectPeek method is one of the “assertion functions” nearly all parsers share. Their primary purpose is to enforce the correctness of the order of tokens by checking the type of the next token. Our expectPeek here checks the type of the peekToken and only if the type is correct does it advance the tokens by calling nextToken. As you’ll see, this is something a parser does a lot.
*/

// expectPeek validates next TokenType and if it is the expected Type, advances position in stmt and return true. 
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

/* 
A Pratt parser’s main idea is the association of parsing functions (which Pratt calls “semantic code”) with token types. Whenever this token type is encountered, the parsing functions are called to parse the appropriate expression and return an AST node that represents it. Each token type can have up to two parsing functions associated with it, depending on whether the token is found in a prefix or an infix position.
*/
type (
	prefixParseFn func() ast.Expression
	infixParseFn func(ast.Expression) ast.Expression
)

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}