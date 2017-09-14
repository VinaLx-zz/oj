namespace ReverseNodesInKGroup {

struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(nullptr) {}
};

class Solution {
  public:
    ListNode *reverseImpl(ListNode *acc, ListNode *head, ListNode *tail) {
        if (head == tail)
            return acc;
        auto next = head->next;
        head->next = acc;
        return reverseImpl(head, next, tail);
    }
    ListNode *reverse(ListNode *head, ListNode *tail) {
        return reverseImpl(tail, head, tail);
    }
    ListNode *reverseKGroup(ListNode *head, int k) {
        if (head == nullptr)
            return head;
        ListNode *tail = head;
        int i = 0;
        for (; tail->next != nullptr && i < k - 1; tail = tail->next, ++i)
            continue;
        if (i == k - 1) {
            tail->next = reverseKGroup(tail->next, k);
            return reverse(head, tail->next);
        }
        return head;
    }
};

} // namespace ReverseNodesInKGroup
