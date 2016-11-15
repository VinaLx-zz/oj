// Given n pairs of parentheses, write a function to generate all combinations
// of well-formed parentheses.
// For example, given n = 3, a solution set is:
// [
//   "((()))",
//   "(()())",
//   "(())()",
//   "()(())",
//   "()()()"
// ]

#include <string>
#include <vector>

using namespace std;

namespace GenerateParentheses {

// depth first search
class Solution {
  public:
    vector<string> generateParenthesis(int n) {
        vector<string> result;
        solve("", 0, 0, n, result);
        return result;
    }

    void
    solve(string current, int left, int now, int n, vector<string> &result) {
        if (not left and now == n) {
            result.push_back(current);
            return;
        }
        if (now < n) {
            solve(current + '(', left + 1, now + 1, n, result);
        }
        if (left) {
            solve(current + ')', left - 1, now, n, result);
        }
    }
};
}