#include <set>
#include <unordered_map>
#include <vector>

using std::vector;
using std::unordered_multimap;
using std::set;
using std::pair;

namespace four_sum {
class Solution {
  public:
    vector<vector<int>> fourSum(vector<int>& nums, int target) {
        unordered_multimap<int, pair<int, int>> m;
        vector<vector<int>> result;
        m.reserve(nums.size() * nums.size() / 2);
        sort(begin(nums), end(nums));
        for (int i = nums.size() - 1; i >= 0;) {
            int a = nums[i];
            for (int j = nums.size() - 1; j > i;) {
                int b = nums[j];
                if (a + b >= target / 2) {
                    m.insert(pair<int, pair<int, int>>{a + b, {i, j}});
                } else {
                    break;
                }
                for (; --j > i and nums[j] == b;)
                    continue;
            }
            if (--i >= 0 and a + a >= target / 2 and nums[i] == a) {
                m.insert(pair<int, pair<int, int>>{a + a, {i, i + 1}});
            }
            for (; i >= 0 and nums[i] == a; --i)
                continue;
        }

        for (int i = 0; i < nums.size();) {
            int a = nums[i];
            for (int j = 0; j < i;) {
                int b = nums[j], target_now = target - a - b;
                if (target_now < target / 2)
                    break;
                auto r = m.equal_range(target_now);
                for (auto iter = r.first; iter != r.second; ++iter) {
                    int ci = iter->second.first, di = iter->second.second;
                    if (i < ci) {
                        result.push_back({a, b, nums[ci], nums[di]});
                    }
                }
                for (; ++j < i and nums[j] == b;)
                    continue;
            }
            if (++i < nums.size() and nums[i] == a and
                target - 2 * a >= target / 2) {
                auto r = m.equal_range(target - 2 * a);
                for (auto iter = r.first; iter != r.second; ++iter) {
                    int ci = iter->second.first, di = iter->second.second;
                    if (i < ci) {
                        result.push_back({a, a, nums[ci], nums[di]});
                    }
                }
            }
            for (; i < nums.size() and nums[i] == a; ++i)
                continue;
        }
        return result;
    }
};
}  // namespace four_sum
