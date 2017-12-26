#include <vector>
#include <deque>
#include <utility>

using std::deque;
using std::pair;
using std::vector;

namespace SlidingWindowMaximum {

class Solution {
public:
    vector<int> maxSlidingWindow(vector<int>& nums, int k) {
        if (k == 1) {
            return nums;
        }
        vector<int> result;
        result.reserve(nums.size() - k + 1);
        deque<pair<int, int>> q;
        for (int i = 0; i < k - 1; ++i) {
            PushDeque(q, {nums[i], i});
        }
        for (int i = k - 1; i < nums.size(); ++i) {
            if (q.back().second <= i - k) {
                q.pop_back();
            }
            PushDeque(q, {nums[i], i});
            result.push_back(q.back().first);
        }
        return result;
    }
    void PushDeque(deque<pair<int, int>>& q, const pair<int, int>& p) {
        for (; not q.empty() and p.first >= q.front().first; ) {
            q.pop_front();
        }
        q.push_front(p);
    }
};

} // namespace SlidingWindowMaximum