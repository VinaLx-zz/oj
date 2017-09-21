#include <string>
#include <vector>

using std::string;
using std::vector;

namespace RegularExpressionMatching {

class Solution {
  public:
    bool isMatch(const string &s, const string &p) {
        visited = vector<vector<bool>>(s.size(), vector<bool>(p.size(), false));
        return isMatchImpl(s, 0, p, 0);
    }

    bool isMatchImpl(const string &s, int i, const string &p, int j) {
        if (i == s.size() && j == p.size()) {
            return true;
        }
        if (j == p.size()) {
            return false;
        }
        if (j + 1 < p.size() && p[j + 1] == '*') {
            return isMatchImpl(s, i, p, j + 1);
        }
        if (i == s.size()) {
            if (p[j] == '*') {
                return isMatchImpl(s, i, p, j + 1);
            }
            return false;
        }
        if (visited[i][j]) {
            return false;
        }
        visited[i][j] = true;
        if (p[j] == '*') {
            if (match(s[i], p[j - 1])) {
                return isMatchImpl(s, i + 1, p, j) ||
                       isMatchImpl(s, i + 1, p, j + 1) ||
                       isMatchImpl(s, i, p, j + 1);
            }
            return isMatchImpl(s, i, p, j + 1);
        }
        if (match(s[i], p[j])) {
            return isMatchImpl(s, i + 1, p, j + 1);
        }
        return false;
    }

    bool match(char c, char p) { return c == p || p == '.'; }

    vector<vector<bool>> visited;
};

} // namespace RegularExpressionMatching
