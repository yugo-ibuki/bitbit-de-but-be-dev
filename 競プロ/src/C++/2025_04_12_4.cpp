#include <iostream>  // 入出力のためのヘッダーファイル
#include <string>    // 文字列操作のためのヘッダーファイル
#include <vector>
#include <algorithm> 
#include <initializer_list>
using namespace std;

/**
問題文
三人兄弟のA君とB君とC君が背くらべをしています。
三人の身長が与えられるので、最も大きい人と最も小さい人の身長差を出力してください。

例えば、
A君の身長が160、B君の身長が154、C君の身長が152であるとします。
このとき最も大きいのはA君で、最も小さいのはC君なので、出力は8になります。
 */
int main() {
  int A, B, C;
  cin >> A >> B >> C;

  vector<int> heights = {A, B, C};

  // .begin() は最初の要素を指すイテレータ
  // .end() は最後の要素の次を指すイテレータ
    // 最後の次は、存在しないので、それが指定された時に1個前が最後ということになる。
    // つまり、以下は失敗する
    // auto it = heights.end();
    // cout << *it;  // 未定義の動作！この位置には有効な値がない
  int max_value = *max_element(heights.begin(), heights.end());
  int min_value = *min_element(heights.begin(), heights.end());

  cout << max_value - min_value << endl;
}
