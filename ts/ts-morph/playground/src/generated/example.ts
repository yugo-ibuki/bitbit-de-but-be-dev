
interface User {
    id: number;
    name: string;
}

function greetUser(user: User): string {
    return `Hello, ${user.name}! Your ID is ${user.id}`;
}

/** ユーザーリストを処理する関数 */
function processUserList(users: User[]): void {
    users.forEach(user => {
        console.log(greetUser(user));
    });
}

/** 非同期でユーザーを取得する関数 */
async function fetchUserById(id: number): Promise<User> {
    // 実際のAPIコールをシミュレート
    const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));
    await delay(1000);
    return { id, name: "Modified User " + id }
}

/** ユーザーデータのバリデーションを行う関数 */
function validateUser(user: User): boolean {

    if (!user.id || !user.name) {
        return false;
    }
    return user.id > 0 && user.name.length > 0;

}
