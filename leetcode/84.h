#include <vector>
#include <stack>
#include <utility>
#include <algorithm>

using std::vector;
using std::stack;
using std::pair;
using std::max;

namespace LargestRectangleInHistogram {

class Solution {
public:
    int reduce(stack<pair<int, int>>& stk, int cur) {
        int result = 0, count = 0;
        for (; not stk.empty() and cur <= stk.top().first; stk.pop()) {
            count += stk.top().second;
            result = max(result, stk.top().first * count);
        }
        stk.push({cur, count + 1});
        return result;
    }
    int largestRectangleArea(vector<int>& heights) {
        int result = 0;
        stack<pair<int, int>> stk;
        for (int i = 0; i < heights.size(); ++i) {
            int cur = heights[i];
            if (stk.empty() or cur > stk.top().first) {
                stk.push({cur, 1});
                continue;
            }
            result = max(result, reduce(stk, cur));
        }
        return max(result, reduce(stk, 0));
    }
};

} // namespace LargestRectangleInHistogram
