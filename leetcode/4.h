#include <algorithm>
#include <vector>

using std::max;
using std::min;
using std::vector;

namespace MedianOfTwoSortedArray {
class Solution {
  public:
    int upperBound(vector<int> &v, int l, int h, int target) {
        if (v[l] > target)
            return l - 1;
        for (; l < h;) {
            int mid = (l + h + 1) / 2;
            if (v[mid] > target)
                h = mid - 1;
            else
                l = mid;
        }
        return h;
    }
    double solve(
        vector<int> &v1, vector<int> &v2, int l1, int h1, int l2, int h2,
        int target) {
        if (l1 > h1)
            return v2[l2 + target];
        if (l2 > h2)
            return v1[l1 + target];
        int c1 = v1[l1], c2 = v2[l2];
        if (target == 0) {
            return c1 < c2 ? c1 : c2;
        }
        int m1 = (l1 + h1) / 2;
        int m2 = upperBound(v2, l2, h2, v1[m1]);
        if (m1 - l1 + 1 + m2 - l2 + 1 >= target + 1) {
            if (c1 < c2)
                ++l1;
            else
                ++l2;
            return solve(v1, v2, l1, m1, l2, m2, target - 1);
        }
        return solve(
            v1, v2, m1 + 1, h1, m2 + 1, h2,
            target - (m1 - l1 + 1) - (m2 - l2 + 1));
    }
    double findMedianSortedArrays(vector<int> &v1, vector<int> &v2) {
        int s1 = v1.size(), s2 = v2.size();
        bool even = (s1 + s2) % 2 == 0;
        int target = (s1 + s2 - 1) / 2;
        double a = solve(v1, v2, 0, s1 - 1, 0, s2 - 1, target);
        return even ? (a + solve(v1, v2, 0, s1 - 1, 0, s2 - 1, target + 1)) / 2
                    : a;
    }
};

class Solution2 {
  public:
    double findMedianSortedArrays(vector<int> &v1, vector<int> &v2) {
        if (v1.size() > v2.size())
            std::swap(v1, v2);
        int l = 0, h = v1.size(), half = (v1.size() + v2.size() + 1) / 2;
        for (; l <= h;) {
            int m1 = (l + h) / 2;
            int m2 = half - m1;
            if (m1 < v1.size() and v1[m1] < v2[m2 - 1]) {
                l = m1 + 1;
            } else if (m1 > 0 and v1[m1 - 1] > v2[m2]) {
                h = m1 - 1;
            } else {
                bool odd = (v1.size() + v2.size()) % 2 == 1;
                int lm = m1 == 0 ? v2[m2 - 1]
                                 : m2 == 0 ? v1[m1 - 1]
                                           : max(v1[m1 - 1], v2[m2 - 1]);
                if (odd)
                    return lm;
                int rm = m1 == v1.size()
                             ? v2[m2]
                             : m2 == v2.size() ? v1[m1] : min(v1[m1], v2[m2]);
                return (lm + rm) / 2.0;
            }
        }
        throw "impossible";
    }
};

} // namespace MedianOfTwoSortedArray
