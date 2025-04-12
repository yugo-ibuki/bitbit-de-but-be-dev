// TypeScriptでの木構造の実装

// 木のノードを表すクラス
class TreeNode<T> {
  value: T;
  children: TreeNode<T>[] = [];

  constructor(value: T) {
    this.value = value;
  }

  // 子ノードを追加するメソッド
  addChild(value: T): TreeNode<T> {
    const newNode = new TreeNode(value);
    this.children.push(newNode);
    return newNode;
  }

  // 深さ優先探索（再帰）
  depthFirstTraversal(callback: (value: T) => void): void {
    // 現在のノードを処理
    callback(this.value);
    
    // 子ノードを再帰的に処理
    for (const child of this.children) {
      child.depthFirstTraversal(callback);
    }
  }

  // 幅優先探索
  breadthFirstTraversal(callback: (value: T) => void): void {
    // キューを使用
    const queue: TreeNode<T>[] = [this];
    
    while (queue.length > 0) {
      // キューから先頭のノードを取り出す
      const currentNode = queue.shift()!;
      
      // 現在のノードを処理
      callback(currentNode.value);
      
      // 子ノードをキューに追加
      for (const child of currentNode.children) {
        queue.push(child);
      }
    }
  }

  // 特定の値を持つノードを探す（深さ優先探索）
  findNode(value: T): TreeNode<T> | null {
    // 現在のノードが探している値を持っている場合
    if (this.value === value) {
      return this;
    }

    // 子ノードを探索
    for (const child of this.children) {
      const found = child.findNode(value);
      if (found) {
        return found;
      }
    }

    // 見つからなかった場合
    return null;
  }
}

// 使用例
function demonstrateTree(): void {
  // ルートノードを作成
  const root = new TreeNode<string>("Root");

  // 子ノードを追加
  const node1 = root.addChild("Node 1");
  const node2 = root.addChild("Node 2");
  const node3 = root.addChild("Node 3");

  // さらに深い階層を追加
  node1.addChild("Node 1.1");
  node1.addChild("Node 1.2");
  node2.addChild("Node 2.1");
  const node22 = node2.addChild("Node 2.2");
  node22.addChild("Node 2.2.1");

  // 深さ優先探索の例
  console.log("深さ優先探索:");
  root.depthFirstTraversal(value => console.log(value));

  // 幅優先探索の例
  console.log("\n幅優先探索:");
  root.breadthFirstTraversal(value => console.log(value));

  // ノード検索の例
  console.log("\nノード検索:");
  const foundNode = root.findNode("Node 2.2");
  if (foundNode) {
    console.log(`ノード "${foundNode.value}" が見つかりました！`);
  } else {
    console.log("ノードが見つかりませんでした");
  }
}

// 実行
demonstrateTree();