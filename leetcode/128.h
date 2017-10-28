#include <algorithm>
#include <numeric>
#include <unordered_map>
#include <vector>

using std::unordered_map;
using std::vector;

namespace LongestConsecutiveSequence {

struct UF {
    UF(int n) : depth(n, 0), parent(n), sz(n, 1) {
        std::iota(begin(parent), end(parent), 0);
    }
    int Root(int a) const {
        if (a < parent.size()) {
            for (; parent[a] != a;) {
                a = parent[a];
            }
        }
        return a;
    }
    void U(int a, int b) {
        int ra = Root(a);
        int rb = Root(b);
        if (ra == rb)
            return;
        if (depth[ra] > depth[rb]) {
            parent[rb] = ra;
            sz[ra] += sz[rb];
        } else if (depth[ra] > depth[rb]) {
            parent[ra] = rb;
            sz[rb] += sz[ra];
        } else {
            parent[ra] = rb;
            sz[rb] += sz[ra];
            ++depth[rb];
        }
    }
    bool F(int a, int b) const {
        return Root(a) == Root(b);
    }
    int Max() const {
        return *std::max_element(begin(sz), end(sz));
    }
    vector<int> sz;
    vector<int> depth;
    vector<int> parent;
};

class Solution {
  public:
    int longestConsecutive(vector<int> &nums) {
        if (nums.empty()) {
            return 0;
        }
        UF uf(nums.size());
        std::unordered_map<int, int> m;
        for (int i = 0; i < nums.size(); ++i) {
            if (m.count(nums[i])) {
                continue;
            }
            m.insert({nums[i], i});
            auto iter = m.find(nums[i] - 1);
            if (iter != end(m)) {
                uf.U(iter->second, i);
            }
            iter = m.find(nums[i] + 1);
            if (iter != end(m)) {
                uf.U(iter->second, i);
            }
        }
        return uf.Max();
    }
};

} // namespace LongestConsecutiveSequence