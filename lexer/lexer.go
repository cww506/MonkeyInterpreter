package lexer

import (
	"monkey-interpreter/token"
)

type Lexer struct {
	input        string
	position     int  // current position in input (points ot current char)
	readPosition int  // current reading position in input (after current char)
	ch           byte // current char under examination
}

func NewLexer(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition += 1
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input){
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

func (l *Lexer) NextToken() token.Token{
	var tok token.Token

	l.skipWhitespace()

	switch l.ch {
	case '=':
		if l.peekChar() == '='{
			ch := l.ch
			l.readChar()
			literal:= string(ch) +string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
			}else{
				tok = newToken(token.ASSIGN, l.ch)
			}
			
	case '-':
		tok =  newToken(token.MINUS, l.ch)
	case '!':
		/* 
		Note that we save l.ch in a local variable before calling l.readChar() again. This way we don’t lose the current character and can safely advance the lexer so it leaves the NextToken() with l.position and l.readPosition in the correct state. If we were to start supporting more two-character tokens in Monkey, we should probably abstract the behaviour away in a method called makeTwoCharToken that peeks and advances if it found the right token. Because those two branches look awfully similar.
		*/
		if l.peekChar() == '='{
			ch := l.ch
			l.readChar()
			literal:= string(ch) +string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
			}else{
				tok = newToken(token.BANG, l.ch)
			}		
	case '/':
		tok =  newToken(token.SLASH, l.ch)
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '>':
		tok =  newToken(token.GT, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch){
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar()
	return tok
}

func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch){
		l.readChar()
	}
	return l.input[position:l.position]
}
/* 
The isLetter helper function just checks whether the given argument is a letter. 
That sounds easy enough, but what’s noteworthy about isLetter is that changing this 
function has a larger impact on the language our interpreter will be able to parse 
than one would expect from such a small function. As you can see, in our case it 
contains the check ch == '_', which means that we’ll treat _ as a letter and allow 
it in identifiers and keywords. That means we can use variable names like foo_bar. 
Other programming languages even allow ! and ? in identifiers. If you want to allow 
that too, this is the place to sneak it in.
*/
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}