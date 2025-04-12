#include <iostream>
#include <string>
#include <sstream>
using namespace std;
 
/**
 * 問題文
 * 1行の計算式が与えられるので、その結果を出力してください。
 * 
 * 与えられる計算式のパターンと対応する出力は以下の表の通りです：
 * 
 * +-------------------+------------------------+---------------------------+
 * | 入力パターン      | 出力                   | 備考                      |
 * +-------------------+------------------------+---------------------------+
 * | A + B            | A+Bの計算結果          |                           |
 * | A - B            | A-Bの計算結果          |                           |
 * | A * B            | A×Bの計算結果          |                           |
 * | A / B            | A÷Bの計算結果          | ・小数点以下は切り捨て     |
 * |                  |                        | ・Bが0の場合はerror出力    |
 * | A ? B            | error                  |                           |
 * | A = B            | error                  |                           |
 * | A ! B            | error                  |                           |
 * +-------------------+------------------------+---------------------------+
 */
int main() {
  string input;
  getline(cin, input);
  stringstream ss(input);
  string A_str, op, B_str;

  ss >> A_str >> op >> B_str;

  if (op == "+") {
    cout << stoi(A_str) + stoi(B_str) << endl;
  } else if (op == "-") {
    cout << stoi(A_str) - stoi(B_str) << endl;
  } else if (op == "*") {
    cout << stoi(A_str) * stoi(B_str) << endl;
  } else if (op == "/") {
    cout << stoi(A_str) / stoi(B_str) << endl;
  } else if (op == "?") {
    cout << "error" << endl;
  } else if (op == "=") {
    cout << "error" << endl;
  } else if (op == "!") {
    cout << "error" << endl;
  }
}