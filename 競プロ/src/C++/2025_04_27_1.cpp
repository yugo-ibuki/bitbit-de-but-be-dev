#include <bits/stdc++.h>
#include <atcoder/all>
using namespace std;
using namespace atcoder;

// https://drken1215.hatenablog.com/entry/2020/10/29/024800 
// 最小二乗法
// 残差平方和とも言われる
// 残差平方和は、データとモデルの「ズレ」がどれくらいあるかを測る方法です。簡単に言うと、「予測値と実際の値の差」を二乗して合計したものです。
int main() {
  int N;
  cin >> N;
  
  // 配列Aの宣言と入力
  vector<int> A(N);
  for (int i = 0; i < N; i++) {
    cin >> A[i];
  }

  // 最小二乗法による最適な値を求める
  long long res = 1LL<<60; // 十分大きな値で初期化
  for (int i = -100; i <= 100; i++) {
    long long tmp = 0;
    for (int j = 0; j < N; j++) {
      tmp += (i - A[j]) * (i - A[j]); // tmpを使用（tempではない）
    }
    res = min(res, tmp);
  }

  // 結果の出力
  cout << res << endl;
}
