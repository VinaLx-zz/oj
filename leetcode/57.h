#include <algorithm>
#include <iostream>
#include <vector>

using std::vector;

namespace InsertInterval {

struct Interval {
    int start;
    int end;
    Interval() : start(0), end(0) {}
    Interval(int s, int e) : start(s), end(e) {}
};

class Solution {
  public:
    vector<Interval> insert(vector<Interval> &intervals, Interval itv) {
        auto e = std::upper_bound(
            begin(intervals), end(intervals), itv.end,
            [](int e, const Interval &i) { return e < i.end; });
        auto s = std::upper_bound(
            rbegin(intervals), rend(intervals), itv.start,
            [](int s, const Interval &i) { return i.start < s; });

        Interval new_itv;
        if (e == end(intervals) or itv.end < e->start) {
            new_itv.end = itv.end;
        } else {
            new_itv.end = e->end;
            ++e;
        }
        if (s == rend(intervals) or itv.start > s->end) {
            new_itv.start = itv.start;
        } else {
            new_itv.start = s->start;
            ++s;
        }

        vector<Interval> result;
        result.reserve(intervals.size());
        std::copy(s, rend(intervals), std::back_inserter(result));
        std::reverse(begin(result), end(result));
        result.push_back(new_itv);
        std::copy(e, end(intervals), std::back_inserter(result));
        return result;
    }
};

} // namespace InsertInterval