
interface Config {
    apiUrl: string;
    timeout: number;
    retryCount: number;
    version: string;
    features: {
        darkMode: boolean;
        notifications: boolean;
        betaFeatures: boolean;
    }
}

interface User {
    id: number;
    name: string;
    role: string;
}

type Endpoints = Record<string, string>;

// ユーザー設定のオブジェクト
const config: Config = {
    apiUrl: "https://api.example.com",
    timeout: 10000,
    retryCount: 3,
    features: {
        darkMode: false,
        notifications: true,
        betaFeatures: true
    },
    version: "1.0.0"
};

// ユーザーリスト
const users: User[] = [
    { id: 1, name: "Alice", role: "admin" },
    { id: 2, name: "Bob", role: "user" },
    { id: 3, name: "Charlie", role: "moderator" }
];

// API エンドポイントの設定
const endpoints: Endpoints = {
    users: "/api/users",
    posts: "/api/posts",
    comments: "/api/comments",
    auth: "/api/auth"
};
