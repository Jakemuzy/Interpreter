#include "val.h"

Value Value::operator+(const Value& op) const {

	if (T == VINT && op.IsInt()) {
		return Value(Itemp + op.GetInt());
	}
	else if (T == VREAL && op.IsReal()) {
		return Value(Rtemp + op.GetReal());
	}
	else if (T == VCHAR && op.IsInt()) {
		return Value(int(Ctemp) + op.GetInt());
	}
	else if ((T == VINT && op.IsChar())) {
		return Value(Itemp + op.GetChar());
	}
	else if (T == VREAL && op.IsInt()) {
		return Value(Rtemp + op.GetInt());
	}
	else if (T == VINT && op.IsReal()) {
		return Value(Itemp + op.GetReal());
	}

	//Int and char/string
	if (T == VREAL && op.IsChar()) {
		return Value(Rtemp + op.GetChar());
	}
	else if (T == VSTRING && op.IsString()) {
		return Value(Stemp + op.GetString());
	}
	else if (T == VSTRING && op.IsChar()) {
		return Value(Stemp + op.GetChar());
	}
	else if (T == VCHAR && op.IsReal()) {
		return Value(float(Ctemp) + op.GetReal());
	}
	else if (T == VCHAR && op.IsString()) {
		return Value(Ctemp + op.GetString());
	}
	else if (T == VCHAR && op.IsChar()) {
		return Value(int(Ctemp) + op.GetChar());
	}

	return Value();
}


Value Value::operator-(const Value& op) const {

	if (T == VINT && op.IsInt()) {
		return Value(Itemp - op.GetInt());
	}
	else if (T == VREAL && op.IsReal()) {
		return Value(Rtemp - op.GetReal());
	}
	else if (T == VCHAR && op.IsInt()) {
		return Value(char(int(Ctemp) - op.GetInt()));
	}
	else if ((T == VINT && op.IsChar())) {
		return Value(char(op.GetChar() - Itemp));
	}
	else if (T == VREAL && op.IsInt()) {
		return Value(Rtemp - op.GetInt());
	}
	else if (T == VINT && op.IsReal()) {
		return Value(Itemp - op.GetReal());
	}

	return Value();
}


Value Value::operator * (const Value& op) const {

	if (T == VINT && op.IsInt()) {
		return Value(Itemp * op.GetInt());
	}
	else if (T == VREAL && op.IsReal()) {
		return Value(float(Rtemp * op.GetReal()));
	}
	else if (T == VCHAR && op.IsInt()) {
		return Value(char(int(Ctemp) * op.GetInt()));		//Can char be multiplied?
	}
	else if ((T == VINT && op.IsChar())) {
		return Value(char(Itemp * op.GetChar()));
	}
	else if (T == VREAL && op.IsInt()) {
		return Value(Rtemp * op.GetInt());
	}
	else if (T == VINT && op.IsReal()) {
		return Value(Itemp * op.GetReal());
	}

	return Value();
}


Value Value::operator / (const Value& op) const {

	if (T == VINT && op.IsInt()) {
		if (op.GetInt() == 0)
			return Value();
		return Value(int(Itemp / op.GetInt()));
	}
	else if (T == VREAL && op.IsReal()) {
		if (op.GetReal() == 0)
			return Value();
		return Value(Rtemp / op.GetReal());
	}
	else if (T == VCHAR && op.IsInt()) {
		if (op.GetInt() == 0)
			return Value();
		return Value(char(int(Ctemp) / op.GetInt()));
	}
	else if ((T == VINT && op.IsChar())) {
		if (op.GetChar() == 0)
			return Value();
		return Value(char(op.GetChar() / Itemp));
	}
	else if (T == VREAL && op.IsInt()) {
		if (op.GetInt() == 0)
			return Value();
		return Value(Rtemp / op.GetInt());
	}
	else if (T == VINT && op.IsReal()) {
		if (op.GetReal() == 0)
			return Value();
		return Value(Itemp / op.GetReal());
	}

	return Value();
}


Value Value::operator % (const Value& op) const {

	if (T == VINT && op.IsInt()) {
		return Value(int(Itemp % op.GetInt()));
	}
	else if (T == VCHAR && op.IsInt()) {
		return Value(int(Ctemp) % op.GetInt());
	}
	else if ((T == VINT && op.IsChar())) {
		return Value(op.GetChar() % Itemp);
	}

	return Value();
}


Value Value::operator==(const Value& op) const {

	//Boolean
	if (T == VBOOL && op.IsBool()) {

		if (Btemp == op.GetBool()) {
			return Value(true);
		}
		return Value(false);
	}

	//Numerics
	if (T == VINT && op.IsInt()) {

		if (Itemp == op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsReal()) {

		if (Rtemp == op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsInt()) {

		if (Ctemp == op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if ((T == VINT && op.IsChar())) {

		if (Itemp == op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsInt()) {

		if (Rtemp == op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VINT && op.IsReal()) {

		if (Itemp == op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}

	//String and char
	if (T == VSTRING && op.IsString()) {

		if (Stemp == op.GetString()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsChar()) {

		if (Ctemp == op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}


	return Value();
}

Value Value::operator!=(const Value& op) const {

	//Boolean
	if (T == VBOOL && op.IsBool()) {

		if (Btemp != op.GetBool()) {
			return Value(true);
		}
		return Value(false);
	}

	//Numerics
	if (T == VINT && op.IsInt()) {

		if (Itemp != op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsReal()) {

		if (Rtemp != op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsInt()) {

		if (Ctemp != op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if ((T == VINT && op.IsChar())) {

		if (Itemp != op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsInt()) {

		if (Rtemp != op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VINT && op.IsReal()) {

		if (Itemp != op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}

	//String and char
	if (T == VSTRING && op.IsString()) {

		if (Stemp != op.GetString()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsChar()) {

		if (Ctemp != op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}

	return Value();
}


Value Value::operator>(const Value& op) const {

	//Boolean
	if (T == VBOOL && op.IsBool()) {

		if (Btemp > op.GetBool()) {
			return Value(true);
		}
		return Value(false);
	}

	//Numerics
	if (T == VINT && op.IsInt()) {

		if (Itemp > op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsReal()) {

		if (Rtemp > op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsInt()) {

		if (Ctemp > op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if ((T == VINT && op.IsChar())) {

		if (Itemp > op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsInt()) {

		if (Rtemp > op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VINT && op.IsReal()) {

		if (Itemp > op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}

	//String and char
	if (T == VSTRING && op.IsString()) {

		if (Stemp > op.GetString()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsChar()) {

		if (Ctemp > op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}

	return Value();
}


Value Value::operator<(const Value& op) const {
	//Boolean
	if (T == VBOOL && op.IsBool()) {

		if (Btemp < op.GetBool()) {
			return Value(true);
		}
		return Value(false);
	}

	//Numerics
	if (T == VINT && op.IsInt()) {

		if (Itemp < op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsReal()) {

		if (Rtemp < op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsInt()) {

		if (Ctemp < op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if ((T == VINT && op.IsChar())) {

		if (Itemp < op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VREAL && op.IsInt()) {

		if (Rtemp < op.GetInt()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VINT && op.IsReal()) {

		if (Itemp < op.GetReal()) {
			return Value(true);
		}
		return Value(false);
	}

	//String and char
	if (T == VSTRING && op.IsString()) {

		if (Stemp < op.GetString()) {
			return Value(true);
		}
		return Value(false);
	}
	else if (T == VCHAR && op.IsChar()) {

		if (Ctemp < op.GetChar()) {
			return Value(true);
		}
		return Value(false);
	}

	return Value();
}


Value Value::operator&&(const Value& op) const {
	if (T == VBOOL && op.IsBool()) {

		if (Btemp && op.GetBool()) {
			return Value(true);
		}
		return Value(false);
	}
	return Value();
}


Value Value::operator||(const Value& op) const {
	if (T == VBOOL && op.IsBool()) {

		if (Btemp || op.GetBool()) {
			return Value(true);
		}
		return Value(false);
	}
	return Value();
}


Value Value::operator!() const {
	if (T == VBOOL) {
		return Value(!Btemp);
	}
	return Value();
}

