#include <string>
#include <vector>

using std::string;
using std::vector;

namespace InterleavingString {

class Solution {
  public:
    bool isInterleave(string s1, string s2, string s3) {
        if (s1.size() + s2.size() != s3.size()) {
            return false;
        }
        visited =
            vector<vector<bool>>(s1.size(), vector<bool>(s2.size(), false));
        return isInterleaveImpl(s1, 0, s2, 0, s3);
    }
    bool isInterleaveImpl(
        const string &s1, int p1, const string &s2, int p2, const string &s3) {
        int p3 = p1 + p2;
        if (p3 == s3.size()) {
            return true;
        }
        if (p1 == s1.size()) {
            return s2.substr(p2) == s3.substr(p3);
        }
        if (p2 == s2.size()) {
            return s1.substr(p1) == s3.substr(p3);
        }
        if (visited[p1][p2])
            return false;
        visited[p1][p2] = true;
        if (s1[p1] == s3[p3] && s2[p2] == s3[p3]) {
            return isInterleaveImpl(s1, p1 + 1, s2, p2, s3) ||
                   isInterleaveImpl(s1, p1, s2, p2 + 1, s3);
        }
        if (s1[p1] == s3[p3]) {
            return isInterleaveImpl(s1, p1 + 1, s2, p2, s3);
        }
        if (s2[p2] == s3[p3]) {
            return isInterleaveImpl(s1, p1, s2, p2 + 1, s3);
        }
        return false;
    }
    vector<vector<bool>> visited;
};

class Solution2 {
  public:
    bool isInterleave(const string& s1, const string& s2, const string& s3) {
        if (s1.size() + s2.size() != s3.size()) {
            return false;
        }
        auto dp = vector<vector<bool>>(
            s1.size() + 1, vector<bool>(s2.size() + 1, false));
        for (int i = 0; i <= s1.size(); ++i) {
            for (int j = 0; j <= s2.size(); ++j) {
                if (i == 0 && j == 0) {
                    dp[i][j] = true;
                    continue;
                }
                int k = i + j - 1;
                bool left = i == 0 ? false : dp[i - 1][j] && s1[i - 1] == s3[k];
                bool up = j == 0 ? false : dp[i][j - 1] && s2[j - 1] == s3[k];
                dp[i][j] = left || up;
            }
        }
        return dp[s1.size()][s2.size()];
    }

};

} // namespace InterleavingString
