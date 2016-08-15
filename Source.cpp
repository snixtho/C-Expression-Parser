#include <iostream>
#include <iomanip>
#include "expr_parser.h"

int main(int argc, char** argv) {
	ExprParser::Converters::InfixToPostfix rpn("43534 + 435 * 236 / 23 - 43 + (345 + 23)");
	ExprParser::DataStructures::StaticList<ExprParser::DataStructures::ExprToken> tokens = rpn.convert();

	std::cout << std::endl;

	ExprParser::Evaluators::RPNEvaluator evaluator(tokens, &rpn);

	std::cout << "Answer is: " << evaluator.evaluate() << std::endl;

	std::cout << std::endl;

	std::cin.get();
	return 0;
}
