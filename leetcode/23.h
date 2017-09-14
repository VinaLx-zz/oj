#include <functional>
#include <queue>
#include <vector>

using std::priority_queue;
using std::vector;

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(nullptr) {}
};

namespace std {

template <>
struct greater<ListNode *> {
    bool operator()(const ListNode *lhs, const ListNode *rhs) const {
        return lhs->val > rhs->val;
    }
};

} // namespace std

namespace MergeKSortedLists {

class Solution {
  public:
      ListNode *mergeKLists(vector<ListNode *> &lists) {
          priority_queue<ListNode*, std::vector<ListNode*>, std::greater<ListNode*>> pq;
          for (auto l : lists) {
              if (l != nullptr) pq.push(l);
          }
          ListNode* head = new ListNode(0);
          auto cur = head;
          for (; not pq.empty(); ) {
              ListNode* now = pq.top();
              pq.pop();
              cur->next = now;
              if (now->next != nullptr) {
                  pq.push(now->next);
              }
              cur = now;
          }
          auto result = head->next;
          delete head;
          return result;
      }
};

} // namespace MergeKSortedLists
