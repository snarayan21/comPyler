# This snippet demonstrates a basic function, lists, subscripting, and while loops.
def reverse_list(arr):
    left = 0
    right = len(arr) - 1
    while (left < right):
        # Swap
        temp = arr[left]
        arr[left] = arr[right]
        arr[right] = temp
        left = left + 1
        right = right - 1
 
    return arr
 
arr = [1, 2, 3, 4, 5, 6, 7]
print(reverse_list(arr))

"""
Expected output:
[7, 6, 5, 4, 3, 2, 1]
"""