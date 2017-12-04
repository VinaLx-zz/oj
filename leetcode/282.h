#include <string>
#include <vector>

using std::string;
using std::vector;

namespace ExpressionAddOperators {

class Solution {
  public:
    vector<string> addOperators(string num, int64_t target) {
        vector<string> result;
        vector<int64_t> accs;
        int64_t first = 0;
        for (int64_t i = 0; i < num.size(); ++i) {
            first = first * 10 + num[i] - '0';
            accs.push_back(first);
            dfs(first, accs, num, i + 1, first, -1, result, target);
            accs.pop_back();
            if (first == 0) {
                break;
            }
        }
        return result;
    }
    void
    dfs(int64_t acc, vector<int64_t> &accs, const std::string &num, int64_t pos,
        int64_t prev, int64_t prevOp, vector<string> &result, int64_t target) {
        if (pos == num.size()) {
            if (acc == target) {
                result.push_back(accToString(accs));
            }
            return;
        }
        int64_t now = 0;
        for (int64_t i = pos; i < num.size(); ++i) {
            now = now * 10 + num[i] - '0';
            for (int64_t thisOp : {-1, -2, -3}) {
                accs.push_back(thisOp);
                accs.push_back(now);
                int64_t nextAcc = 0;
                int64_t nextPrev = 0;
                if (thisOp != -3) {
                    nextAcc = calc(acc, now, thisOp);
                    nextPrev = now;
                } else {
                    switch (prevOp) {
                    case -1:
                        nextAcc = acc - prev + prev * now;
                        break;
                    case -2:
                        nextAcc = acc + prev - prev * now;
                        break;
                    default:
                        throw "error";
                    }
                    nextPrev = prev * now;
                    thisOp = prevOp;
                }
                dfs(nextAcc, accs, num, i + 1, nextPrev, thisOp, result,
                    target);
                accs.pop_back();
                accs.pop_back();
            }
            if (now == 0) {
                break;
            }
        }
    }
    static string accToString(const vector<int64_t> &accs) {
        string result;
        for (auto i : accs) {
            if (i >= 0) {
                result += std::to_string(i);
            } else {
                switch (i) {
                case -1:
                    result.push_back('+');
                    break;
                case -2:
                    result.push_back('-');
                    break;
                case -3:
                    result.push_back('*');
                    break;
                default:
                    throw "error";
                }
            }
        }
        return result;
    }
    static int64_t stoi(const std::string &num, int64_t start, int64_t end) {
        int64_t res = 0;
        for (int64_t i = start; i < end; ++i) {
            res = res * 10 + num[i] - '0';
        }
        return res;
    }
    static int64_t calc(int64_t lhs, int64_t rhs, int64_t op) {
        switch (op) {
        case -1:
            return lhs + rhs;
        case -2:
            return lhs - rhs;
        case -3:
            return lhs * rhs;
        default:
            throw "error";
        }
    }
};


} // namespace ExpressionAddOperators
