import { json, useLoaderData } from "@remix-run/react"

export const loader = () => {
    console.log('env api_key: ', process.env.API_KEY || 'not found');
    
    return json({
        test: 'hello',
        env: process.env.API_KEY || 'not found',
    })
}

export default function Test() {
    const data = useLoaderData()
    console.log('data: ', data);
    console.log('dddd');
    
    return <div>Test</div>
}