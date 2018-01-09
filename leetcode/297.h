#include <string>
#include <sstream>
#include <utility>
#include <cctype>

using std::to_string;
using std::string;
using std::ostringstream;
using std::pair;
using std::isdigit;

namespace SerializeAndDeserializeBinaryTree {

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x): val(x), left(nullptr), right(nullptr) {}
};

class Codec {
public:
    // Encodes a tree to a single string.
    string serialize(TreeNode* root) {
        if (root == nullptr) {
            return ".";
        }
        ostringstream oss;
        oss << root->val << ',' << serialize(root->left) << ',' << serialize(root->right);
        return oss.str();
    }

    // Decodes your encoded data to tree.
    TreeNode* deserialize(string data) {
        return deserialize_impl(data, 0).first;
    }
    pair<TreeNode*, int> deserialize_impl(const string& data, int idx) {
        if (data[idx] == ',') {
            ++idx;
        }
        if (data[idx] == '.') {
            return {nullptr, idx + 1};
        }
        auto pv = parse_int(data, idx);
        auto left = deserialize_impl(data, pv.second);
        auto right = deserialize_impl(data, left.second);
        auto node = new TreeNode(pv.first);
        node->left = left.first;
        node->right = right.first;
        return {node, right.second};
    }
    pair<int, int> parse_int(const string& data, int idx) {
        bool flag = false;
        if (data[idx] == '-') {
            flag = true;
            ++idx;
        }
        int value = 0;
        for (; idx < data.size() and isdigit(data[idx]); ) {
            value = value * 10 + data[idx++] - '0';
        }
        if (flag) {
            value = -value;
        }
        return {value, idx};
    }
};

} // namespace SerializeAndDeserializeBinaryTree