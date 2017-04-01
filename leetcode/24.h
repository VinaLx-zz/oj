// Given a linked list, swap every two adjacent nodes and return its head.
//
// For example,
// Given 1->2->3->4, you should return the list as 2->1->4->3.
//
// Your algorithm should use only constant space. You may not modify the values
// in the list, only nodes itself can be changed.

namespace swap_nodes_in_pairs {

struct ListNode {
    int val;
    ListNode* next;
    ListNode(int x) : val(x), next(nullptr) {}
};

class Solution {
  public:
    ListNode* swapPairs(ListNode* head) {
        if (not head or not head->next) {
            return head;
        }
        ListNode *ret = head->next, *prev = head, *cur = head;
        cur->next = cur->next->next;
        ret->next = cur;
        for (cur = cur->next; cur and cur->next; cur = cur->next) {
            ListNode* temp = cur->next;
            cur->next = cur->next->next;
            prev->next = temp;
            temp->next = cur;
            prev = cur;
        }
        return ret;
    }
};

}  // namespace swap_nodes_in_pairs
