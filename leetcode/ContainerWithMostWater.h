// Given n non-negative integers a1, a2, ..., an, where each represents a point
// at coordinate (i, ai). n vertical lines are drawn such that the two endpoints
// of line i is at (i, ai) and (i, 0). Find two lines, which together with
// x-axis forms a container, such that the container contains the most water.

#include <vector>

using namespace std;

namespace ContainerWithMostWater {

// basic thought:
// initialize the boundary to the left most and right most edge
// continuously move the short line to the middle
// whenever the edge meet a longer line than the original one, calculate the
// volume and maintain the largest volume
class Solution {
  public:
    int maxArea(vector<int> &height) {
        int left = 0, right = height.size() - 1,
            volume = (right - left) * min(height[left], height[right]);
        for (; left < right;) {
            if (height[left] < height[right]) {
                for (int current = height[left];
                     left < right and height[left] <= current;)
                    ++left;
            } else {
                for (int current = height[right];
                     left < right and height[right] <= current;)
                    --right;
            }
            volume =
                max(volume, (right - left) * min(height[left], height[right]));
        }
        return volume;
    }
};

} // ContainerWithMostWater
