// abc042 C - Iroha's Obsession
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
using namespace std;

set<int> D;
bool isValid(int n) {
    // nが0になるまで繰り返す。
    while (n) {
        // D.countでDにn % 10が含まれているかを確認する。
        if (D.count(n % 10)) return false; // 禁止された数字が含まれているか。例えば、1234の場合 % 10をすると4になり、Dに4が含まれているかを確認する。
        n /= 10; // 1234の場合、10で割ると123になり、また10で割ると12になり、また10で割ると1になり、また10で割ると0になる。
    }
    return true;
}

int main() {
    int N, M;
    cin >> N >> M;

    for (int i = 0; i < M; i++) {
        int a;
        cin >> a;
        D.insert(a); // setにinsertすることでスペース区切りの数字をsetに格納する。
    }

    for (int i = N;; ++i) {
        if (isValid(i)) {
            cout << i << endl;
            return 0;
        }
    }
}
