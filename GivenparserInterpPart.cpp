
#include "parserInterp.h"
#include <algorithm>

map<string, bool> defVar;
map<string, Token> SymTable;
map<string, Value> TempsResults; //Container of temporary locations of Value objects for results of expressions, variables values and constants 
queue <Value> * ValQue; //declare a pointer variable to a queue of Value objects

bool error = false;
map<int, bool> printIf;
map<int, bool> printElse;
int nestingLevel = 0;
Token VarType;

namespace Parser {
	bool pushed_back = false;
	LexItem	pushed_token;

	static LexItem GetNextToken(istream& in, int& line) {
		if( pushed_back ) {
			pushed_back = false;
			return pushed_token;
		}
		return getNextToken(in, line);
	}

	static void PushBackToken(LexItem & t) {
		if( pushed_back ) {
			abort();
		}
		pushed_back = true;
		pushed_token = t;	
	}

}

static int error_count = 0;

int ErrCount()
{
    return error_count;
}

void ParseError(int line, string msg)
{
	++error_count;
	cout << line << ": " << msg << endl;
}

//Type Convert
map<Token, ValType> typeConvertBack = { {ICONST, VINT}, {RCONST,VREAL}, {SCONST,VSTRING}, {CCONST,VCHAR}, {BCONST,VBOOL}, {ERR, VERR} };
map<ValType, Token> typeConvert = { {VINT,ICONST}, {VREAL,RCONST}, {VSTRING, SCONST}, {VCHAR, CCONST}, {VBOOL, BCONST}, {VERR, ERR} };


//This is very very wrong, tempVal should compare its theoretical type to its actual DeclStmt vartype
//This is the most ass solution I have ever come up with, but I genuinely didn't know how else to implement it 
Value assignValue(LexItem& idtok, int line) {
	Token tok = idtok.GetToken();
	Value tempVal;

	//Ident
	if (tok == IDENT) {
		//Using nonexistent variable
		if (TempsResults.find(idtok.GetLexeme()) == TempsResults.end()) {
			ParseError(line, "Using Uninitialized Variable");
			error = true;
		}
		else if (TempsResults[idtok.GetLexeme()].IsErr()) {
			ParseError(line, "Using Uninitialized Variable");
			error = true;
		}

		tempVal = TempsResults[idtok.GetLexeme()];
		//tempVal.SetType(typeConvertBack[SymTable[idtok.GetLexeme()]]);		//Probably don't need this since just set equal
	}
	else {

		//If Variable type is already defined in DeclStmt
		if (VarType != ERR) {
			switch (VarType) {
			case INT:
				tempVal.SetInt(stoi(idtok.GetLexeme()));
				tempVal.SetType(VINT);
				break;
			case FLOAT:
				tempVal.SetReal(stod(idtok.GetLexeme()));
				tempVal.SetType(VREAL);
				break;
			case BOOL:
				if (idtok.GetLexeme() == "true")
					tempVal.SetBool(true);
				else if (idtok.GetLexeme() == "false")
					tempVal.SetBool(false);
				tempVal.SetType(VBOOL);
				break;
			case CHAR:
				tempVal.SetChar(idtok.GetLexeme()[0]);
				tempVal.SetType(VCHAR);
				break;
			case STRING:
				tempVal.SetString(idtok.GetLexeme());
				tempVal.SetType(VSTRING);
				break;
			default:
				cerr << "Error assigning value " << VarType << " " << idtok.GetLexeme() << "\n";
				break;
			}
		}
		else {
			// Every other data type
				switch (tok) {
				case ICONST:
					tempVal.SetInt(stoi(idtok.GetLexeme()));
					tempVal.SetType(VINT);
					break;
				case RCONST:
					tempVal.SetReal(stod(idtok.GetLexeme()));
					tempVal.SetType(VREAL);
					break;
				case TRUE:
					tempVal.SetBool(true);
					tempVal.SetType(VBOOL);
					break;
				case FALSE:
					tempVal.SetBool(false);
					tempVal.SetType(VBOOL);
					break;
				case BCONST:
					if (idtok.GetLexeme() == "true")
						tempVal.SetBool(true);
					else if (idtok.GetLexeme() == "false")
						tempVal.SetBool(false);
					tempVal.SetType(VBOOL);
					break;
				case CCONST:
					tempVal.SetChar(idtok.GetLexeme()[0]);
					tempVal.SetType(VCHAR);
					break;
				case SCONST:
					tempVal.SetString(idtok.GetLexeme());
					tempVal.SetType(VSTRING);
					break;
				default:
					cerr << "Error assigning value " << idtok.GetToken() << " " << idtok.GetLexeme() << "\n";
					break;
				}
		}

		
	}
	//cout << idtok.GetLexeme() << " " << TempsResults[idtok.GetLexeme()] << endl;
	return tempVal;
}

bool typeMismatch(Value v1, Value v2) {

	//Boolean
	if (v1.IsBool() && v2.IsBool())
		return true;

	//Numerics
	if (v1.IsInt() && v2.IsInt()) 
		return true;
	else if (v1.IsReal() && v2.IsReal())
		return true;
	else if (v1.IsChar() && v2.IsInt())
		return true;
	else if ((v1.IsInt() && v2.IsChar()))
		return true;
	else if (v1.IsReal() && v2.IsInt())
		return true;
	else if (v1.IsInt() && v2.IsReal())
		return true;

	//String and char
	if (v1.IsString() && v2.IsString())
		return true;
	else if (v1.IsChar() && v2.IsChar())
		return true;

	return false;
}


//PrintStmt:= PRINT (ExpreList) 
bool PrintStmt(istream& in, int& line) {
	LexItem t;

	t = Parser::GetNextToken(in, line);
	if (t != PRINT) {
		Parser::PushBackToken(t);
		return false;
	}

	t = Parser::GetNextToken(in, line);
	if (t != LPAREN) {

		ParseError(line, "Missing Left Parenthesis");
		error = true;
		Parser::PushBackToken(t);
		return false;
	}

	ValQue = new queue<Value>;
	bool ex = ExprList(in, line);
	
	if (!ex) {
		ParseError(line, "Missing expression list after Print");
		return false;
	}

	t = Parser::GetNextToken(in, line);
	if (t != RPAREN) {

		ParseError(line, "Missing Right Parenthesis");
		error = true;
		return false;
	}

	//Determines if it should print

	//Evaluate: print out the list of expressions values
	while (!(*ValQue).empty()) {
		cout << (*ValQue).front();
		(*ValQue).pop();
	}
	cout << "\n";

	ValQue = nullptr;		//Possibly wrong, revisit
	return true;
}//End of PrintStmt


//ExprList:= Expr {,Expr}
bool ExprList(istream& in, int& line) {

	Value retVal; //TEMPORTY I THINK

	bool status = false;
	status = Expr(in, line, retVal);

	if (!status) {
		return false;
	}

	LexItem tok = Parser::GetNextToken(in, line);

	if (tok == COMMA) {
		status = ExprList(in, line);
	}
	else if (tok.GetToken() == ERR) {
		cout << "(" << tok.GetLexeme() << ")" << endl;
		return false;
	}
	else {
		Parser::PushBackToken(tok);
		return true;
	}
	return status;
}//End of ExprList


bool Prog(istream& in, int& line) {
	ValQue = nullptr;

	Token progCheck = Parser::GetNextToken(in, line).GetToken();
	if (progCheck != PROGRAM) {
		ParseError(line, "Missing Program keyword");
		return false;
	}

	Token identCheck = Parser::GetNextToken(in, line).GetToken();
	if (identCheck != IDENT) {
		ParseError(line, "Missing Program name");
		return false;
	}

	//Proceeds to CompStmt, if returns false, return false
	bool cmpStntCheck = CompStmt(in, line);
	if (!cmpStntCheck) {
		ParseError(line, "Invalid Program");
		return false;
	}
	cout << "(DONE)\n";
	return cmpStntCheck;

}

bool CompStmt(istream& in, int& line) {
	LexItem lbraceCheck = Parser::GetNextToken(in, line);

	if (lbraceCheck.GetToken() != LBRACE) {
		Parser::PushBackToken(lbraceCheck);	//not needed
		return false;
	}

	//In between the braces are a list of statements, if any of these are false, return false
	//HAVE TO PUSH BACK TOKEN IF ANY STMT FAILS 
	if (!StmtList(in, line)) {
		ParseError(line, "Incorrect statement list");
		return false;
	}

	LexItem rbraceCheck = Parser::GetNextToken(in, line);
	if (rbraceCheck.GetToken() != RBRACE) {
		//ParseError(line, "No RBRACE detected");
		Parser::PushBackToken(rbraceCheck);
		return false;
	}
	//If all of these are correct return true;
	return true;	//Maybe return stmtListCheck
}

bool StmtList(istream& in, int& line) {
	//Don't need to getToken here, cuz Stmt does it

	if (!Stmt(in, line)) {	//IF first Stmt failed, then theres no stmts, cannot continue
		if (error) {
			ParseError(line, "Syntactic error in statement list");
			return false;
		}
		return false;
	}

	bool cont = StmtList(in, line);	//Calls itself, until theres no longer any Stmts
	if (error) {
		return false;
	}
	while (cont) {
		if (error) {
			return false;
		}
		cont = StmtList(in, line);
	}

	return true;
}

vector<string> declared;
bool Stmt(istream& in, int& line) {

	//Decl

	if (DeclStmt(in, line)) {
		LexItem semicolCheck = Parser::GetNextToken(in, line);
		if (semicolCheck.GetToken() != SEMICOL) {
			Parser::PushBackToken(semicolCheck);
			ParseError(line, "Missing semicolon at the end of Statement.");
			error = true;		//Guess it's not an error?
			return false;
		}
		for (string var : declared)
			//cout << "Initialization of the variable " << var << " in the declaration statement at line " << line << "\n";
		declared.clear();
		return true;
	}
	else {
		if (error) {
			//This should behave correclty, however it doesn't so have to move to varlist
			ParseError(line, "Invalid declaration statement.");
			return false;
		}

	}

	//Control
	if (ControlStmt(in, line)) {
		return true;
	}
	else {
		if (error) {
			ParseError(line, "Invalid control statment.");
			return false;
		}
	}

	//Comp
	if (CompStmt(in, line)) {
		return true;
	}
	else {

	}


	//If not any of the valid Stmt, return false
	LexItem rBraceCheck = Parser::GetNextToken(in, line);
	Parser::PushBackToken(rBraceCheck);

	if (rBraceCheck != RBRACE)
		error = true;
	return false;
}

//Gets variable type, and then names of the variables (VarList)
bool DeclStmt(istream& in, int& line) {

	vector<Token> validToks = { INT, FLOAT, BOOL, CHAR, STRING };

	LexItem declStmtCheck = Parser::GetNextToken(in, line);

	//If first val not a Int, Float, Bool, Char, String, return false
	if (find(validToks.begin(), validToks.end(), declStmtCheck.GetToken()) == validToks.end())
	{
		Parser::PushBackToken(declStmtCheck);
		return false;
	}

	LexItem idtok = declStmtCheck;	//TEMPORARY I THINK
	VarType = declStmtCheck.GetToken();
	//If second val, not a varList, return false;
	if (!VarList(in, line, idtok)) {
		//if error
		ParseError(line, "Incorrect variable in Declaration Stmt");
		error = true;
		return false;
	}

	//Add type checking and compatability
	VarType = ERR;
	return true;
}
bool Var(istream& in, int& line, LexItem& idtok) {
	LexItem varCheck = Parser::GetNextToken(in, line);
	if (varCheck.GetToken() == IDENT) {
		idtok = varCheck;
		return true;
	}
	else if (varCheck.GetToken() == IF){
		Parser::PushBackToken(varCheck);
		return false;
	}

	Parser::PushBackToken(varCheck);
	return false;
	
}

//List of variables, serparated by comma
bool VarList(istream& in, int& line, LexItem& idtok) {

	if (!Var(in, line, idtok)) {
		return false;
	}
	//If variable already delcared
	if (defVar.find(idtok.GetLexeme()) != defVar.end()) {
		ParseError(line, "Variable Redefinition");
		error = true;
		return false;
	}

	//Temporary null until it gets to primary expression, where it is assigned
	Value retVal;

	//Check for optional equal sign and expr
	LexItem equalCheck = Parser::GetNextToken(in, line);
	if (equalCheck.GetToken() == ASSOP) {					//If there is an equal sign, check if the next Token is a Expr
		//if theres a bad expression, return false, else check for other varaibles
		if (!Expr(in, line, retVal)) {
			
			ParseError(line, "Incorrect initializiation for a variable");
			error = true;
			return false;
		}
		declared.push_back(idtok.GetLexeme());
	}
	else {	// if none continue to check for other variables
		Parser::PushBackToken(equalCheck);
	}

	//Assigning
	map<Token, Token> toType = { {INT, ICONST},{BOOL, BCONST}, {FLOAT, RCONST}, {CHAR, CCONST}, {STRING, SCONST}, {IDENT, IDENT} };
	defVar[idtok.GetLexeme()] = 0;
	SymTable[idtok.GetLexeme()] = toType[VarType];
	TempsResults[idtok.GetLexeme()] = retVal;

	//Checking for more
	//Check for comma, if none return true
	LexItem commaCheck = Parser::GetNextToken(in, line);
	if (commaCheck.GetToken() != COMMA) {
		Parser::PushBackToken(commaCheck);

		if (commaCheck.GetToken() != SEMICOL) {
			return false;
		}
		return true;
	}

	//Determines if theres any more variables if there is a comma

	//USE SIMILAR TRICK AS EXPRLIST
	bool cont = VarList(in, line, idtok);	//Calls itself, until theres no longer any Vars
	while (cont) {
		if (error) {
			return false;
		}
		cont = VarList(in, line, idtok);
	}
	if (error) {
		error = true;
		return false;
	}

	return true;
}

bool Expr(istream& in, int& line, Value& retVal) {
	if (!LogANDExpr(in, line, retVal)) {
		return false;
	}

	Value temp = retVal;
	//If there is an or 
	LexItem orCheck = Parser::GetNextToken(in, line);
	if (orCheck.GetToken() == OR) {
		if (Expr(in, line, retVal)) {
			retVal = (temp || retVal);
			return true;	//Think this is right? not sure
		}
		else {
			//If theres an || but not LogANDExpr, return false
			Parser::PushBackToken(orCheck);
			return false;
		}

	}

	Parser::PushBackToken(orCheck);
	return true;
}

bool LogANDExpr(istream& in, int& line, Value& retVal) {
	if (!EqualExpr(in, line, retVal)) {
		return false;
	}
	Value temp = retVal;

	//If there is an or 
	LexItem andCheck = Parser::GetNextToken(in, line);
	if (andCheck.GetToken() == AND) {
		//IF theres an || and LogAndExpr
		if (LogANDExpr(in, line, retVal)) {
			if (!typeMismatch(temp, retVal)) {
				ParseError(line, "Invalid operands for logical AND operator.");
				error = true;
				return false;
			}
			return true;	//Think this is right? not sure
		}
		else {
			if (error) {
				return false;
			}
			//If theres an || but not LogANDExpr, return false
			Parser::PushBackToken(andCheck);
			return false;
		}

		retVal = (temp && retVal);
	}

	Parser::PushBackToken(andCheck);
	return true;
}
bool EqualExpr(istream& in, int& line, Value& retVal) {
	if (!RelExpr(in, line, retVal)) {
		return false;
	}
	Value temp = retVal;

	//If there is an or 
	LexItem eqNeqCheck = Parser::GetNextToken(in, line);
	Token eqNeq = eqNeqCheck.GetToken();
	if (eqNeqCheck.GetToken() == EQ || eqNeqCheck.GetToken() == NEQ) {

		if (!RelExpr(in, line, retVal)) {
			Parser::PushBackToken(eqNeqCheck);
			return false;
		}
		else {
			eqNeqCheck = Parser::GetNextToken(in, line);
			if (eqNeqCheck.GetToken() == EQ || eqNeqCheck.GetToken() == NEQ) {
				ParseError(line, "Illegal Equality Expression");
				error = true;
				return false;
			}
			Parser::PushBackToken(eqNeqCheck);

			//Evaluate
			switch (eqNeq) {
			case EQ:
				retVal = (temp == retVal);
				break;
			case NEQ:
				retVal = (temp != retVal);
				break;
			default:
				break;
			}
		}
	}
	else {
		Parser::PushBackToken(eqNeqCheck);
	}

	return true;
}
bool RelExpr(istream& in, int& line, Value& retVal) {
	if (!AddExpr(in, line, retVal)) {
		return false;
	}
	Value temp = retVal;

	//If there is an or 
	LexItem lthanGthanCheck = Parser::GetNextToken(in, line);
	Token tempToken = lthanGthanCheck.GetToken();
	if (lthanGthanCheck.GetToken() == LTHAN || lthanGthanCheck.GetToken() == GTHAN) {

		if (!AddExpr(in, line, retVal)) {
			Parser::PushBackToken(lthanGthanCheck);
			return true;
		}
		else {

			lthanGthanCheck = Parser::GetNextToken(in, line);
			if (lthanGthanCheck.GetToken() == LTHAN || lthanGthanCheck.GetToken() == GTHAN) {
				ParseError(line, "Illegal relational Expression");
				error = true;
				return false;
			}
		}
		Parser::PushBackToken(lthanGthanCheck);
		if (!typeMismatch(temp, retVal)) {
			ParseError(line, "Invalid operands for relational operators");
			error = true;
			return false;
		}

		
		//Evaluate stmt
		switch (tempToken) {
		case LTHAN:
			retVal = (temp < retVal);
			break;
		case GTHAN:
			retVal = (temp > retVal);
			break;
		default:
			break;
		}


		//return 
		return true;
	}

	if (!typeMismatch(temp, retVal)) {
		ParseError(line, "Invalid operands for relational operators");
		error = true;
		return false;
	}

	Parser::PushBackToken(lthanGthanCheck);
	return true;
}
bool AddExpr(istream& in, int& line, Value& retVal) {
	if (!MultExpr(in, line, retVal)) {
		return false;
	}
	Value temp = retVal;
	
	LexItem plusMinusCheck = Parser::GetNextToken(in, line);
	if (plusMinusCheck.GetToken() == PLUS || plusMinusCheck.GetToken() == MINUS) {

		if (AddExpr(in, line, retVal)) {	//MultExpr if wrong
			//Actual addition
			switch (plusMinusCheck.GetToken()) {
			case PLUS:
				retVal = (temp + retVal);
				break;
			case MINUS:
				retVal = (temp - retVal);
				break;
			default:
				break;
			}
			return true;	
		}
		else {
			Parser::PushBackToken(plusMinusCheck);
			return false;
		}

	}
	Parser::PushBackToken(plusMinusCheck);
	return true;
}
bool MultExpr(istream& in, int& line, Value& retVal) {
	if (!UnaryExpr(in, line, retVal)) {
		return false;
	}
	jump:
	Value temp = retVal;
	
	LexItem divMultRemCheck = Parser::GetNextToken(in, line);
	if (divMultRemCheck.GetToken() == DIV || divMultRemCheck.GetToken() == MULT || divMultRemCheck.GetToken() == REM) {

		if (UnaryExpr(in, line, retVal)) {
			if (!typeMismatch(temp, retVal)) {
				ParseError(line, "Illegal operand type for the operation.");
				error = true;
				return false;
			}
			//Rem without ints
			if (divMultRemCheck.GetToken() == REM && (temp % retVal).GetType() == VERR) {
				ParseError(line, "Invalid operand for the REM operator");
				error = true;
				return false;
			}
			//Divide by 0 
			if (divMultRemCheck.GetToken() == DIV && (temp / retVal).GetType() == VERR) {
				ParseError(line, "Run-Time Error-Illegal division by Zero");
				error = true;
				return false;
			}

			//Actually do the operation
			switch (divMultRemCheck.GetToken()) {
			case DIV:
				retVal = (temp / retVal);
				break;
			case MULT:
				retVal = (temp * retVal);
				break;
			case REM:
				retVal = (temp % retVal);
				break;
			default:
				break;
			}
			goto jump;		//Actually retarded solution but whatevs, don't feel like fixing my messy code LOL!
		}
		else {
			//Error being mismatch operator types
			if (error) {
				return false;
			}
			ParseError(line, "Missing operand after operator");	//Might have to put this everywhere
			return false;
		}

	}

	Parser::PushBackToken(divMultRemCheck);
	return true;
}
bool UnaryExpr(istream& in, int& line, Value& retVal) {
	LexItem plusMinusNotCheck = Parser::GetNextToken(in, line);
	int sign = 4;	// 2 = not, 1 = plus, 0 = minus

	if (plusMinusNotCheck.GetToken() == MINUS) {

		sign = 0;
	}
	else if (plusMinusNotCheck.GetToken() == PLUS) {

		sign = 1;
	}
	else if (plusMinusNotCheck.GetToken() == NOT) {

		sign = 2;
	}
	else {
		Parser::PushBackToken(plusMinusNotCheck);
	}

	//Check if its a primary Expr
	if (PrimaryExpr(in, line, sign, retVal)) {
		return true;
	}

	return false;
}
bool PrimaryExpr(istream& in, int& line, int sign, Value& retVal) {
	vector<Token> validToks = { IDENT, ICONST, RCONST, SCONST, BCONST, CCONST };

	LexItem primaryExprCheck = Parser::GetNextToken(in, line);

	//If token not ident, int, float, string, bool, or char
	//Or if not an expression, return false
	auto valueType = find(validToks.begin(), validToks.end(), primaryExprCheck.GetToken());
	if (valueType != validToks.end())
	{
		//Token type = *valueType;
		//cout << "TOKEN TYPE: " << type << " " << primaryExprCheck.GetLexeme() << " \n";

		retVal = assignValue(primaryExprCheck, line);
		if (sign == 2 && !retVal.IsBool()) {
			ParseError(line, "Illegal Operand Type for NOT Operator");
			error = true;
			return false;
		}
		else if ((sign == 0 || sign == 1) && (retVal.IsBool() || retVal.IsString()) ) {
			ParseError(line, "Illegal Operand Type for Sign Operator");
			error = true;
			return false;
		}
		
		//ValQue for PrintStmt
		if (ValQue != nullptr)
			(*ValQue).push(retVal);

		//Returns
		if (error) 
			return false;
		return true;
	}
	else if (primaryExprCheck.GetToken() == DONE) {
		cout << "(DONE)\n";
		return true;
	}

	if (primaryExprCheck.GetToken() == LPAREN) {
		if (!Expr(in, line, retVal)) {	//Mayhaps not the right retval
			ParseError(line, "Missing expression after Left Parenthesis");
			return false;
		}
		LexItem rParenCheck = Parser::GetNextToken(in, line);
		if (rParenCheck.GetToken() == RPAREN) {
			return true;
		}
		ParseError(line, "Missing right Parenthesis after expression");
		error = true;
		return false;
	}

	Parser::PushBackToken(primaryExprCheck);
	return false;
}

bool ControlStmt(istream& in, int& line) {

	//Assignment stmt
	if (AssignStmt(in, line)) {
		LexItem semicolCheck = Parser::GetNextToken(in, line);
		if (semicolCheck.GetToken() != SEMICOL) {
			Parser::PushBackToken(semicolCheck);
			line--;		//Subtract line because for some reason, pushing back doesn't subtract line
			ParseError(line, "Missing semicolon at the end of Statement.");	//Some reason line num is off
			error = true;
			return false;
		}
		return true;
	}
	else {
		if (error) {
			ParseError(line, "Incorrect Assignment Statement");
			return false;
		}
	}

	//If stmt
	if (IfStmt(in, line)) {
		return true;
	}
	else {
		if (error) {
			ParseError(line, "Incorrect IF Statement");
			return false;
		}
	}

	//Print stmt
	if (PrintStmt(in, line)) {
		LexItem semicolCheck = Parser::GetNextToken(in, line);
		if (semicolCheck.GetToken() != SEMICOL) {
			Parser::PushBackToken(semicolCheck);
			ParseError(line, "Missing semicolon at the end of Statement.");	//Some reason line num is off
			error = true;
			return false;
		}
		return true;
	}
	else {
		if (error) {
			ParseError(line, "Incorrect Print Statement");
			return false;
		}
	}

	return false;
}

bool IfStmt(istream& in, int& line) {
	//IF
	LexItem ifCheck = Parser::GetNextToken(in, line);
	if (ifCheck.GetToken() != IF) {
		Parser::PushBackToken(ifCheck);
		return false;
	}

	LexItem lParenCheck = Parser::GetNextToken(in, line);
	if (lParenCheck.GetToken() != LPAREN) {
		return true;
	}
	//Nesting level
	nestingLevel++;
	//cout << "in IfStmt then-clause at nesting level: " << nestingLevel << "\n";

	Value retVal;		//TEMPORARY I THINK
	//Expr
	if (!Expr(in, line, retVal)) {
		ParseError(line, "Missing if statement condition");
		error = true;
		return false;
	}
	
	//If operands aren't the right type
	if (retVal.GetType() == VERR) {
		ParseError(line, "Invalid operands for relational operators");
		error = true;
		return false;
	}
	//If expression doesn't evaluate to a boolean
	else if (retVal.GetType() != VBOOL) {
		ParseError(line, "Invalid type for If statement condition");
		error = true;
		return false;
	}
	//At this point it's true so evaluate statement
	LexItem rParenCheck = Parser::GetNextToken(in, line);
	if (rParenCheck.GetToken() != RPAREN) {
		return false;
	}

	//Evaluating whether or not if or else stmt gets activated
	if (retVal.GetBool() == true) {
		if (!Stmt(in, line)) {
			ParseError(line, "Invalid If Expression");
			return false;
		}
	}
	else {
		int ifCount = 0;
		LexItem t = Parser::GetNextToken(in, line);
		while ((t.GetToken() != ELSE) || !(ifCount == 0)) {
			if (t.GetToken() == ELSE)
				ifCount--;
			if (t.GetToken() == IF)
				ifCount++;
			t = Parser::GetNextToken(in, line);
		}
		Parser::PushBackToken(t);

	}

	LexItem elseCheck = Parser::GetNextToken(in, line);
	if (elseCheck == ELSE && retVal.GetBool() == false) {
		//Check for stmt
		//cout << "in IfStmt else-clause at nesting level: " << nestingLevel << "\n";
		if (!Stmt(in, line)) {
			ParseError(line, "Invalid Else Expression ");
			return false;
		}
	}
	else {
		//Else without braces
		if (elseCheck == ELSE) {
			elseCheck = Parser::GetNextToken(in, line);
			if (elseCheck.GetToken() == LBRACE) {
				while (elseCheck.GetToken() != RBRACE) {
					elseCheck = Parser::GetNextToken(in, line);
				}
			}
			else {
				while (elseCheck.GetToken() != SEMICOL) {
					elseCheck = Parser::GetNextToken(in, line);
				}
			}
		} 
		else {	//Not an else stmt, just an if
			Parser::PushBackToken(elseCheck);
		}
	}
		
	nestingLevel = 1;
	return true;
}

bool AssignStmt(istream& in, int& line) {
	vector<Token> validAssignments = { ASSOP, ADDASSOP, SUBASSOP, MULASSOP, DIVASSOP, REMASSOP };
	
	//Var
	LexItem varCheck;
	if (!Var(in, line, varCheck)) {
		return false;
	}

	//Check if theres not a lvalue for rvalue to be assigned to 
	if (defVar.find(varCheck.GetLexeme()) == defVar.end()) {
		ParseError(line, "Undeclared Variable");
		ParseError(line, "Missing Left-Hand Side Variable in Assignment statement");
		error = true;	//Probably incorrect
		return false;
	}

	LexItem assignmentCheck = Parser::GetNextToken(in, line);
	if (find(validAssignments.begin(), validAssignments.end(), assignmentCheck.GetToken()) == validAssignments.end()) {
		Parser::PushBackToken(assignmentCheck);
		ParseError(line, "Missing Assignment Operator");
		error = true;
		return false;
	}

	//Evaluate Value 
	Value retVal;
	if (!Expr(in, line, retVal)) {
		ParseError(line, "Missing Expression in Assignment Statement");
		error = true;
		return false;
	}

	//Type Mismatch		
	if (TempsResults[varCheck.GetLexeme()].IsErr()) {	//Only was error because undefined, now reassign type
		if (assignmentCheck.GetToken() != ASSOP) {
			ParseError(line, "Invalid Expression evaluation in Assignment Statement");
			error = true;
			return false;
		}
		TempsResults[varCheck.GetLexeme()].SetType(typeConvertBack[SymTable[varCheck.GetLexeme()]]);
	}

	Value temp = TempsResults[varCheck.GetLexeme()];
	if (!typeMismatch(temp, retVal)) {
		ParseError(line, "Illegal mixed-mode assignment operation");
		error = true;
		return false;
	}

	//Actual assignment
	switch (assignmentCheck.GetToken()) {
	case ASSOP:
		TempsResults[varCheck.GetLexeme()] = retVal;
		TempsResults[varCheck.GetLexeme()].SetType(retVal.GetType());
		break;
	case ADDASSOP:
		TempsResults[varCheck.GetLexeme()] = (TempsResults[varCheck.GetLexeme()] + retVal);
		TempsResults[varCheck.GetLexeme()].SetType(retVal.GetType());
		break;
	case SUBASSOP:
		TempsResults[varCheck.GetLexeme()] = (TempsResults[varCheck.GetLexeme()] - retVal);
		TempsResults[varCheck.GetLexeme()].SetType(retVal.GetType());
		break;
	case MULASSOP:
		TempsResults[varCheck.GetLexeme()] = (TempsResults[varCheck.GetLexeme()] * retVal);
		TempsResults[varCheck.GetLexeme()].SetType(retVal.GetType());
		break;
	case DIVASSOP:
		TempsResults[varCheck.GetLexeme()] = (TempsResults[varCheck.GetLexeme()] / retVal);
		TempsResults[varCheck.GetLexeme()].SetType(retVal.GetType());
		break;
	case REMASSOP:
		TempsResults[varCheck.GetLexeme()] = (TempsResults[varCheck.GetLexeme()] % retVal);
		TempsResults[varCheck.GetLexeme()].SetType(retVal.GetType());
		break;
	default:
		break;
	}
	
	//Assigns var with its name
	defVar[varCheck.GetLexeme()] == 1;
	return true;
}