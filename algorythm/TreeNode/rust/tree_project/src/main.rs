// Rustでの木構造の実装
use std::collections::VecDeque;

// TreeNodeは木構造のノードを表す
#[derive(Debug)]
struct TreeNode {
    value: String,
    children: Vec<TreeNode>,
}

impl TreeNode {
    // 新しいTreeNodeを作成する
    fn new(value: &str) -> Self {
        TreeNode {
            value: value.to_string(),
            children: Vec::new(),
        }
    }

    // 子ノードを追加する
    fn add_child(&mut self, value: &str) -> &mut TreeNode {
        let child = TreeNode::new(value);
        self.children.push(child);
        self.children.last_mut().unwrap()
    }

    // 深さ優先探索（再帰）
    fn depth_first_traversal<F>(&self, callback: &mut F)
    where
        F: FnMut(&str),
    {
        // 現在のノードを処理
        callback(&self.value);

        // 子ノードを再帰的に処理
        for child in &self.children {
            child.depth_first_traversal(callback);
        }
    }

    // 幅優先探索
    fn breadth_first_traversal<F>(&self, callback: &mut F)
    where
        F: FnMut(&str),
    {
        // キューを使用
        let mut queue = VecDeque::new();
        queue.push_back(self);

        while let Some(current_node) = queue.pop_front() {
            // 現在のノードを処理
            callback(&current_node.value);

            // 子ノードをキューに追加
            for child in &current_node.children {
                queue.push_back(child);
            }
        }
    }

    // 特定の値を持つノードを探す（深さ優先探索）
    fn find_node(&self, value: &str) -> Option<&TreeNode> {
        // 現在のノードが探している値を持っている場合
        if self.value == value {
            return Some(self);
        }

        // 子ノードを探索
        for child in &self.children {
            if let Some(found) = child.find_node(value) {
                return Some(found);
            }
        }

        // 見つからなかった場合
        None
    }
}

// 使用例
fn main() {
    // ルートノードを作成
    let mut root = TreeNode::new("Root");

    // 子ノードを追加（可変参照を一時的に保持）
    let node1 = root.add_child("Node 1");
    node1.add_child("Node 1.1");
    node1.add_child("Node 1.2");

    let node2 = root.add_child("Node 2");
    node2.add_child("Node 2.1");
    let node22 = node2.add_child("Node 2.2");
    node22.add_child("Node 2.2.1");

    root.add_child("Node 3");

    // 深さ優先探索の例
    println!("深さ優先探索:");
    root.depth_first_traversal(&mut |value| {
        println!("{}", value);
    });

    // 幅優先探索の例
    println!("\n幅優先探索:");
    root.breadth_first_traversal(&mut |value| {
        println!("{}", value);
    });

    // ノード検索の例
    println!("\nノード検索:");
    if let Some(found_node) = root.find_node("Node 2.2") {
        println!("ノード \"{}\" が見つかりました！", found_node.value);
    } else {
        println!("ノードが見つかりませんでした");
    }
}
