#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#define rep(i, a, b) for (int i = a; i < b; i++)
using namespace std;

/**
 * 問題文：カードゲームの最適戦略
 * 
 * 高橋君は以下のカードを持っています：
 * - 青いカード：N枚（文字列s_iが書かれている）
 * - 赤いカード：M枚（文字列t_iが書かれている）
 * 
 * ゲームのルール：
 * 1. 高橋君は文字列を1つ言います
 * 2. 全てのカードを確認します
 * 3. 言った文字列と完全に一致する青いカード1枚につき1円もらえます
 * 4. 言った文字列と完全に一致する赤いカード1枚につき1円失います
 * 
 * 注意事項：
 * - 文字列は完全一致の場合のみカウントされます
 *   （例：「atcoder」と言った場合、「atcoderr」「atcode」「btcoder」などは不一致）
 * - 同じ文字列が複数のカードに書かれていることもあります
 * 
 * 問題：高橋君は最大で差し引き何円もらうことができるでしょうか？
 * 
 * https://atcoder.jp/contests/abc091/tasks/abc091_b
 */

int N, M; // 変数の定義
string S[101], T[101]; // 文字列の配列

int main() {
  cin >> N;
  // rep はマクロとして以下のようにファイルトップに定義されている
  // #define rep(i, a, b) for (int i = a; i < b; i++)
  rep(i, 0, N) cin >> S[i]; // 青いカードの入力
  cin >> M;
  // T[i] は配列の指すポインタに格納することを意味する
  rep(i, 0, M) cin >> T[i]; // 赤いカードの入力

  int ans = 0;
  rep(si, 0, N) {
      int point = 0;
      rep(i, 0, N) if (S[i] == S[si]) point++;
      rep(i, 0, M) if (T[i] == S[si]) point--;
      ans = max(ans, point);
  }
  cout << ans << endl;
}
