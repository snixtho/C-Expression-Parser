#pragma once

#ifndef EXPR_PARSER_H
#define EXPR_PARSER_H

#include <cstring>
#include <iostream>
#include <vector>
#include <algorithm>

namespace ExprParser {

	// Holds very simple data structures for the parser
	namespace DataStructures {

		// Simple generic stack
		template <typename T, int STATIC_STACKSIZE = 128>
		class Stack {
		public:
			Stack() : m_isEmpty(true), m_size(0) { _init(); }

			// Push a value on top of the stack
			void push(T value) {
				if ((int)(m_sp + 1) - (int)(T*)m_stackdata >= STATIC_STACKSIZE * sizeof(T))
				{
					perror("ExprParser Stack Overflow.");
					return; // stack overflow
				}
				else
				{
					if (!m_isEmpty)
					{
						++m_sp;
					}

					++m_size;

					m_isEmpty = false;
					*m_sp = value;
				}
			}

			// Pop a value from the top of the stack
			T pop() {
				T value = *m_sp;

				if (!m_isEmpty)
					--m_size;

				if ((int)m_sp - (int)(T*)m_stackdata != 0)
				{
					--m_sp;
				}
				else
				{
					m_isEmpty = true;
				}

				return value;
			}

			// Return the value at the top of the stack
			inline T top() {
				return *m_sp;
			}

			// Returns true if the stack is empty, false if not
			inline bool isEmpty() {
				return m_isEmpty;
			}

			// Returns the current size of the stack
			inline int size() {
				return m_size;
			}
		private:
			inline void _init() {
				memset(m_stackdata, 0, sizeof(T) * STATIC_STACKSIZE);
				m_sp = (T*)m_stackdata;
			}

			T m_stackdata[STATIC_STACKSIZE];
			T* m_sp;
			bool m_isEmpty;
			int m_size;
		};

		// simple generic static list
		template <typename T>
		class StaticList {
		public:
			StaticList(int maxSize) : m_maxSize(maxSize), m_size(0) {
				m_array = new T[maxSize];
				memset(m_array, 0, sizeof(T) * maxSize);
				m_ap = m_array;
			}

			// copy constructor
			StaticList(StaticList& other) : m_maxSize(other.m_maxSize), m_size(0) {
				m_array = new T[other.m_maxSize];
				memset(m_array, 0, sizeof(T) * other.m_maxSize);
				m_ap = m_array;

				for (int i = 0; i < other.size(); i++)
				{
					if (m_size != 0)
					{
						++m_ap;
					}

					++m_size;

					*m_ap = other[i];
				}
			}

			~StaticList() {
				delete[] m_array;
			}

			// Add an element at the end of the list
			void push(T value) {
				if (m_size > m_maxSize)
				{
					perror("ExprParser List overflow.");
					return; // list overflow.
				}
				else
				{
					if (m_size != 0)
					{
						++m_ap;
					}

					++m_size;

					*m_ap = value;
				}
			}

			// Get element from pos
			inline void get(int pos) { return m_array[pos]; }

			// get elment from index
			inline T &operator[] (int index) {
				return m_array[index];
			}

			// Get current size of the list
			inline int size() { return m_size; }

		private:
			T* m_array;
			T* m_ap;
			int m_size;
			int m_maxSize;
		};

		// Operator associativity identifiers
		enum class OperatorAssociativity {
			Left,
			Right
		};

		// Expression type identifiers
		enum class ExprType {
			Operand,
			Operator,
			Function,
			Variable,
			Unknown
		};

		// Holds expression operators
		class ExprOperator {
		public:
			typedef long double(*OpFunction)(long double, long double);

		public:
			ExprOperator(std::string op, OpFunction func) : m_op(op), m_func(func), m_assoc(OperatorAssociativity::Left), m_precedence(2) {}
			ExprOperator(std::string op, OperatorAssociativity assoc, int precedence, OpFunction func) : m_op(op), m_func(func), m_precedence(precedence), m_assoc(assoc) {}

			// caller for the operator function
			operator OpFunction() { return m_func; }

			// Get the associativity of the operator
			inline OperatorAssociativity getAssociativity() { return m_assoc; }

			// Get the actual operator
			inline const std::string& getOp() { return m_op; }

			// Get the operator precedence
			inline int getPrecedence() { return m_precedence; }

		private:
			std::string m_op;
			OpFunction m_func;
			OperatorAssociativity m_assoc;
			int m_precedence;
		};

		// Holds expression functions
		class ExprFunction {
			typedef long double(*FunctionType)(StaticList<long double>);

		public:
			ExprFunction(std::string name, FunctionType func, int nargs = 2) : m_name(name), m_func(func), m_nargs(nargs) {}

			// call the function
			operator FunctionType() { return m_func; }

			// get the name of the function
			inline const std::string getName() { return m_name; }

			// Get the number of arguments this function supports
			inline int argCount() { return m_nargs; }

		private:
			FunctionType m_func;
			std::string m_name;
			int m_nargs;
		};

		// holds a token
		struct ExprToken {
			ExprType type;
			ExprFunction* func;
			ExprOperator* op;
			std::string valueStr;
			long double valueNum;
		};

	}

	namespace Checks {
		// Checks if a character is any of the specials below 0x20
		inline bool is_superspecial(char c) {
			return c < 0x20;
		}

		// Returns true if c is a whitespace
		inline bool is_whitespace(char c) {
			return c == ' ';
		}

		// returns true if c is character 0-9
		inline bool is_numeric(char c) {
			return 0x30 <= c && c <= 0x39;
		}

		// Returns true if character is a dot (.)
		inline bool is_float_separator(char c) {
			return c == '.';
		}

		// Returns true if c is a parenthesis character.
		inline bool is_parenthesis(char c)
		{
			return c == '(' || c == ')';
		}

		// Returns true if the character is a comma (,)
		inline bool is_function_separator(char c) {
			return c == ',';
		}

		// Returns if the character is a alpha char [a-zA-Z]
		inline bool is_alpha(char c) {
			return (c >= 0x41 && c <= 0x5A) || (c >= 0x61 && c <= 0x7a);
		}

		// Returns true if the character is a valid operator character
		inline bool is_operator_char(char c) {
			return !is_superspecial(c) && !is_whitespace(c) && !is_numeric(c) && !is_parenthesis(c);
		}

		// Check if the negative sign is the start of a negative number
		inline bool is_negative(const char* start, const char* ptr) {
			if (*ptr == '-' && is_numeric(*(ptr + 1)))
			{
				if (ptr == start)
					return true;

				--ptr;
				while (*ptr == ' ' && ptr != start)
					--ptr;

				if (is_operator_char(*ptr))
					return true;
			}

			return false;
		}

		// Returns -1 if the entities assembles a variable, 1 if it is a function and 0 if invalid.
		inline int multichar_op(const char* ptr) {
			if (!is_alpha(*ptr))
			{ // variables cant start with a number
				return 0;
			}

			while (*ptr != 0)
			{
				if (!is_alpha(*ptr) && !is_numeric(*ptr) && !is_whitespace(*ptr))
				{
					if (*ptr == '(')
						return 1;
					else
						return -1;
				}

				++ptr;
			}

			return -1;
		}

		// operator associativity, -1 = left, 1 = right, 0 = unknown
		inline int op_associativity(std::string &op) {
			if (op == "+")
				return -1;
			else if (op == "-")
				return -1;
			else if (op == "*")
				return -1;
			else if (op == "/")
				return -1;
			else if (op == "%")
				return -1;
			else if (op == "^")
				return 1;

			return 0;
		}

		// operator preceedence, -1 = unknown
		inline int op_precedence(std::string &op)
		{
			if (op == "+")
				return 2;
			else if (op == "-")
				return 2;
			else if (op == "*")
				return 3;
			else if (op == "/")
				return 3;
			else if (op == "%")
				return 3;
			else if (op == "^")
				return 4;
			else if (op == ")")
				return 1;
			else if (op == "(")
				return 1;

			return -1;
		}

		inline bool is_function(std::string &op) {
			if (op == "max")
				return true;
			else if (op == "min")
				return true;
			else if (op == "sqrt")
				return true;
			else if (op == "rand")
				return true;

			return false;
		}
	}

	namespace Converters {
		// Base class for expression converters
		class Converter {
		public:
			Converter(const char* input) : m_in(input) {}

			// Used for the conversion algorithm implementation
			virtual DataStructures::StaticList<DataStructures::ExprToken> convert(const char* in = nullptr) = 0;

			// Add a valid operator to the tokenizer
			inline void addOperator(DataStructures::ExprOperator* op) {
				m_operators.push_back(op);
			}

			// Get the pointer to the operator info/function.
			inline DataStructures::ExprOperator* getOpPointer(std::string op) {
				auto it = std::find_if(m_operators.begin(), m_operators.end(), [&op](DataStructures::ExprOperator* obj) { return obj->getOp() == op; });

				if (it != m_operators.end())
				{
					return *it;
				}
				else
				{
					return nullptr;
				}
			}

			// add a valid function to the tokenizer
			inline void addFunction(DataStructures::ExprFunction* op) {
				m_functions.push_back(op);
			}

			// Get the pointer to the function info/method from its name.
			inline DataStructures::ExprFunction* getFuncPointer(std::string func) {
				auto it = std::find_if(m_functions.begin(), m_functions.end(), [&func](DataStructures::ExprFunction* obj) { return obj->getName() == func; });

				if (it != m_functions.end())
				{
					return *it;
				}
				else
				{
					return nullptr;
				}
			}



		protected:
			const char* m_in;
			std::vector<DataStructures::ExprOperator*> m_operators;
			std::vector<DataStructures::ExprFunction*> m_functions;
		};
	}

	class TokenFactory {
	public:
		// Create a token from the value based on its type
		static DataStructures::ExprToken CreateToken(DataStructures::ExprType type, std::string value, Converters::Converter* converter) {
			DataStructures::ExprToken _token;
			_token.type = type;

			switch (type)
			{
			case DataStructures::ExprType::Function:
				_token.valueStr = value;
				_token.func = converter->getFuncPointer(value);
				break;
			case DataStructures::ExprType::Operand:
			{ // todo: implement proper overflow detection for floats
				long double valuen = 0;
				long double valuef = 0;
				long double maxNum = pow(2, sizeof(long double) * 8);
				bool isFloat = false;
				bool floatPos = 1;

				for (int i = 0; i < value.size(); i++)
				{
					if (value[i] == '.')
					{
						isFloat = true;
						continue;
					}
					if (isFloat)
					{
						long double div = pow(10, floatPos);
						valuef = valuef + (value[i] - 48) / div;
						++floatPos;
					}
					else
					{
						valuen = valuen * 10 + (value[i] - 48);
					}
				}

				_token.valueNum = valuen + valuef;

				if (_token.valueNum > maxNum)
					_token.valueNum = _token.valueNum - (_token.valueNum - maxNum);

				break;
			}
			case DataStructures::ExprType::Operator:
				_token.op = converter->getOpPointer(value);
				_token.valueStr = value;
				break;
			case DataStructures::ExprType::Variable:
				_token.valueStr = value;
				break;
			}

			return _token;
		}
	};

	namespace Converters {
		// Infix to RPN (reverse polish notation)
		class InfixToPostfix : public Converter {
		public:
			InfixToPostfix(const char* input) : Converter(input) {
				typedef typename DataStructures::ExprOperator ExprOperator;
				typedef typename DataStructures::ExprFunction ExprFunction;

				/* OPERATORS */

				// addition
				addOperator(new ExprOperator("+", DataStructures::OperatorAssociativity::Left, 2, [](long double a, long double b) {
					return a + b;
				}));
				// subtraction
				addOperator(new ExprOperator("-", DataStructures::OperatorAssociativity::Left, 2, [](long double a, long double b) {
					return a - b;
				}));
				// multiplication
				addOperator(new ExprOperator("*", DataStructures::OperatorAssociativity::Left, 3, [](long double a, long double b) {
					return a * b;
				}));
				// division
				addOperator(new ExprOperator("/", DataStructures::OperatorAssociativity::Left, 3, [](long double a, long double b) {
					return a / b;
				}));
				// modulus
				addOperator(new ExprOperator("%", DataStructures::OperatorAssociativity::Left, 3, [](long double a, long double b) {
					return (long double)((int)a % (int)b);
				}));
				// power of
				addOperator(new ExprOperator("^", DataStructures::OperatorAssociativity::Right, 4, [](long double a, long double b) {
					return pow(a, b);
				}));
				// opening parenthesis 
				addOperator(new ExprOperator("(", DataStructures::OperatorAssociativity::Left, 1, [](long double a, long double b) {
					return (long double)0;
				}));
				// closing parenthesis
				addOperator(new ExprOperator(")", DataStructures::OperatorAssociativity::Left, 1, [](long double a, long double b) {
					return (long double)0;
				}));

				/* FUNCTIONS */

				// max(a, b)
				addFunction(new ExprFunction("max", [](DataStructures::StaticList<long double> args) {
					return args[0] > args[1] ? args[0] : args[1];
				}));

				// min(a, b)
				addFunction(new ExprFunction("min", [](DataStructures::StaticList<long double> args) {
					return args[0] < args[1] ? args[0] : args[1];
				}));

				// sqrt(a)
				addFunction(new ExprFunction("sqrt", [](DataStructures::StaticList<long double> args) {
					return sqrt(args[0]);
				}, 1));

				// rand()
				addFunction(new ExprFunction("rand", [](DataStructures::StaticList<long double> args) {
					return sqrt(args[0]);
				}, 0));
			}

			DataStructures::StaticList<DataStructures::ExprToken> convert(const char* in = nullptr) override {
				// todo: fix multiplication being ignore if it is before parenthesis, example: a * (b + c)

				const char* input = nullptr;

				if (in == nullptr)
					input = m_in;
				else
					input = in;
				DataStructures::StaticList<DataStructures::ExprToken> convertedList(strlen(input));
				DataStructures::Stack<std::string> opStack;
				const char* ptr = input;
				const char* ptrstart = ptr;
				while (*ptr != 0)
				{
					bool next_already = false;

					if (!Checks::is_superspecial(*ptr) && !Checks::is_whitespace(*ptr))
					{
						if (Checks::is_numeric(*ptr) || (*ptr == '-' && Checks::is_negative(ptrstart, ptr))) // detection: integer/float
						{ // is a number/float
							std::string snum;

							if (*ptr == '-')
							{// correct for negative numbers
								snum += *ptr;
								++ptr;
							}

							while (*ptr != 0 && (Checks::is_numeric(*ptr) || Checks::is_float_separator(*ptr)))
							{
								snum += *ptr;
								++ptr;
								next_already = true;
							}

							//convertedList.push(snum);
							convertedList.push(TokenFactory::CreateToken(DataStructures::ExprType::Operand, snum, this));
						}
						else if (Checks::multichar_op(ptr) == -1) // detection: variable
						{ // token is a variable
							std::string var;
							while (*ptr != 0 && (Checks::is_alpha(*ptr) || Checks::is_numeric(*ptr)))
							{
								var += *ptr;
								++ptr;
								next_already = true;
							}

							// convertedList.push(var);
							convertedList.push(TokenFactory::CreateToken(DataStructures::ExprType::Variable, var, this));
						}
						else if (Checks::is_function_separator(*ptr)) // detection: function argument separator
						{ // dump operator stack until opening parenthesis is found, 
							// dont dump the parenthesis for support for multi-argument functions
							while (!opStack.isEmpty() && opStack.top() != "(")
							{
								// convertedList.push(opStack.pop());
								convertedList.push(TokenFactory::CreateToken(DataStructures::ExprType::Operator, opStack.pop(), this));
							}
						}
						else if (*ptr == '(') // detection: parenthesis opening
						{ // start of parenthesis calc
							std::string op;
							op = *ptr;
							opStack.push(op);
						}
						else if (*ptr == ')')
						{
							while (!opStack.isEmpty() && opStack.top() != "(") // detection: parenthesis closing
							{ // dump operators until opening parenthesis is found
								// convertedList.push(opStack.pop());
								convertedList.push(TokenFactory::CreateToken(DataStructures::ExprType::Operator, opStack.pop(), this));
							}

							if (!opStack.isEmpty())
							{
								opStack.pop();

								if (getFuncPointer(opStack.top()) != nullptr /* Checks::is_function(opStack.top()) */)
								{ // pop possible function
									// convertedList.push(opStack.pop());
									convertedList.push(TokenFactory::CreateToken(DataStructures::ExprType::Function, opStack.pop(), this));
								}
							}
							else
							{
								perror("Left parenthesis is not closed correctly, terminating ...");
								return DataStructures::StaticList<DataStructures::ExprToken>(1);
							}
						}
						else
						{ // is an operator
							std::string op;
							while (*ptr != 0 && Checks::is_operator_char(*ptr) && !Checks::is_negative(ptrstart, ptr))
							{
								if (Checks::is_operator_char(op[0]) && !Checks::is_alpha(op[0]) && Checks::multichar_op(ptr) != 0)
								{
									break;
								}

								op += *ptr;
								++ptr;
								next_already = true;
							}

							if (getFuncPointer(op) != nullptr /* Checks::is_function(op) */) // detection: function
							{// if function
								opStack.push(op);
							}
							else
							{
								/* int perceedence = Checks::op_precedence(op);
								int associativity = Checks::op_associativity(op); */
								DataStructures::ExprOperator* opp = getOpPointer(op);

								if (opp != nullptr)
								{// valid operator
									int perceedence = opp->getPrecedence();
									DataStructures::OperatorAssociativity associativity = opp->getAssociativity();

									while (!opStack.isEmpty())
									{
										int top_perceedence = Checks::op_precedence(opStack.top());

										if ((associativity == DataStructures::OperatorAssociativity::Left && perceedence <= top_perceedence)
											|| (associativity == DataStructures::OperatorAssociativity::Right && perceedence < top_perceedence)) // detection: operator
										{
											// convertedList.push(opStack.pop());
											convertedList.push(TokenFactory::CreateToken(DataStructures::ExprType::Operator, opStack.pop(), this));
										}
										else
										{
											break;
										}
									}

									opStack.push(op);
								}
								else
								{ // illegal operator in string
									perror("Invalid operator found, terminating ...");
									return DataStructures::StaticList<DataStructures::ExprToken>(1);
								}
							}
						}
					}

					if (!next_already)
						++ptr;
				}

				while (!opStack.isEmpty())
				{
					std::string op = opStack.pop();
					// convertedList.push(opStack.pop());
					convertedList.push(TokenFactory::CreateToken(DataStructures::ExprType::Operator, opStack.pop(), this));
				}

				return convertedList;
			}
		};
	}

	namespace Evaluators {

		// Base class for evaluators
		class Evaluator {
		public:
			Evaluator(DataStructures::StaticList<DataStructures::ExprToken> tokens, Converters::Converter* converter) : m_tokens(tokens), m_converter(converter) {}

			virtual long double evaluate() = 0;

		protected:
			Converters::Converter* m_converter;
			DataStructures::StaticList<DataStructures::ExprToken> m_tokens;
		};

		// Reverse Polish Notation evaluator
		class RPNEvaluator : public Evaluator {
		public:
			RPNEvaluator(DataStructures::StaticList<DataStructures::ExprToken> tokens, Converters::Converter* converter) : Evaluator(tokens, converter) {}

			long double evaluate() override {
				DataStructures::Stack<long double> resultStack;

				for (int i = 0; i < m_tokens.size(); i++)
				{
					DataStructures::ExprToken token = m_tokens[i];

					if (token.type == DataStructures::ExprType::Operand)
					{
						resultStack.push(token.valueNum);
					}
					else if (token.type == DataStructures::ExprType::Operator)
					{
						if (resultStack.size() < 2)
						{
							perror("Expression contains insufficient number of values for the operation.");
							return 0;
						}

						long double b = resultStack.pop();
						long double a = resultStack.pop();

						long double result = (*token.op)(a, b);

						resultStack.push(result);
					}
					else if (token.type == DataStructures::ExprType::Function)
					{
						if (resultStack.size() < token.func->argCount())
						{
							perror("Expression contains insufficient number of values for the function.");
							return 0;
						}

						DataStructures::StaticList<long double> args(token.func->argCount());

						for (int i = 0; i < token.func->argCount(); i++)
						{ // todo: reverse this
							args.push(resultStack.pop());
						}

						long double result = (*token.func)(args);

						resultStack.push(result);
					}
				}

				if (resultStack.size() == 1)
				{
					return resultStack.pop();
				}
				else
				{
					perror("The expression has too many values to evaluate.");
					return 0;
				}
			}
		};

	}
}

#endif
