#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
using namespace std;
#define rep(i, a, b) for (int i = a; i < b; i++)

int main() {
    string S;
    cin >> S;
   // この問題は、アルファベット26文字しかないので、O(N)で十分
    // 過半数を超える場合のみ、この条件が考慮される。
    // そして、過半数を超えるのは、隣り合う文字が同じ場合か、1つしか空いていない場合のみ。
    for (int i = 0; i+1 < S.size(); ++i) {
        // 隣り合う文字が同じ場合
        if (S[i] == S[i+1]) l = i+1, r = i+2;
        // 隣り合う文字が違う場合
        if (i+2 < S.size() && S[i] == S[i+2]) l = i+1, r = i+3;
        // 2つ以上空く場合はスキップされる。なぜなら、2つ以上空く場合は過半数になり得ないから。
}
