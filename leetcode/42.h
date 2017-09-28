#include <stack>
#include <utility>
#include <vector>

using std::pair;
using std::stack;
using std::vector;

namespace TrappingRainWater {

class Solution {
  public:
    int trap(vector<int> &heights) {
        int water = 0;
        stack<pair<int, int>> stk;
        for (int i = 0; i < heights.size(); ++i) {
            auto &cur = heights[i];
            if (stk.empty()) {
                stk.push({i, cur});
                continue;
            }
            int collected = 0;
            for (; not stk.empty() and stk.top().second <= cur; stk.pop()) {
                water +=
                    (stk.top().second - collected) * (i - stk.top().first - 1);
                collected = stk.top().second;
            }
            if (not stk.empty()) {
                water += (cur - collected) * (i - stk.top().first - 1);
            }
            stk.push({i, cur});
        }
        return water;
    }
};

} // namespace TrappingRainWater