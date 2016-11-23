// Given two numbers represented as strings, return multiplication of the numbers as a string.

// Note:

//     The numbers can be arbitrarily large and are non-negative.
//     Converting the input string to integer is NOT allowed.

#include <string>
#include <vector>

using std::vector;
using std::string;

namespace MultiplyString {

/**
 * high precision multiplication
 */
class Solution {
public:
    string multiply(string num1, string num2) {
        if (num1 == "0" or num2 == "0") return "0";
        vector<int> result(num1.size() + num2.size(), 0);
        for (int i = 0; i < num1.size(); ++i) {
            for (int j = 0; j < num2.size(); ++j) {
                result[i+j+1] += (num1[i] - '0') * (num2[j] - '0');
            }
        }
        for (int i = result.size() - 1; i > 0; --i) {
            result[i-1] += result[i] / 10;
            result[i] %= 10;
        }
        int start = result.front() ? 0 : 1;
        string answer;
        answer.reserve(result.size());
        for (int i = start; i < result.size(); ++i) {
            answer.push_back(result[i] + '0');
        }
        return answer;
    }
};

} // namespace multiply string