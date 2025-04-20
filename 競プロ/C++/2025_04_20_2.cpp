#include <iostream>
#include <vector>
#include <string>
using namespace std;

/*
 * 問題文
 * N 個の整数 A_1, A_2, ..., A_N が与えられます。
 * N 個の整数を合計した値を求めてください。
 */
int main() {
    int N;
    cin >> N;
    int sum = 0;
    for (int i = 0; i < N; i++) {
        int a;
        cin >> a;
        sum += a;
    }

    cout << sum << endl;
}
