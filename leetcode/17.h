#include <string>
#include <vector>

using std::vector;
using std::string;

namespace letter_combinations_of_a_phone_number {

// dfs and backtracking
class Solution {
  public:
    const char numbers[10][5] = {"",    "",    "abc",  "def", "ghi",
                                 "jkl", "mno", "pqrs", "tuv", "wxyz"};
    vector<string> letterCombinations(string digits) {
        if (digits.empty())
            return {};
        vector<string> result;
        dfs(result, digits, 0, "");
        return result;
    }
    void dfs(
        vector<string>& result, const string& digits, int index,
        const string& current) {
        if (index == digits.size()) {
            result.push_back(current);
            return;
        }
        const char* alphas = numbers[digits[index] - '0'];
        for (; *alphas;) {
            dfs(result, digits, index + 1, current + *alphas++);
        }
    }
};

}  // namespace letter_combinations_of_a_phone_number
