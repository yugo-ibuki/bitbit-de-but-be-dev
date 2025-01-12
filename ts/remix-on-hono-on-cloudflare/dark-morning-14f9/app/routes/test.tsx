import { json, useLoaderData } from "@remix-run/react"

export const loader = () => {
    const key = environment?.API_KEY
    console.log('env api_key: ', key || 'not found');
    
    return json({
        test: 'hello',
        env: key || 'not found',
    })
}

export default function Test() {
    const data = useLoaderData()
    console.log('data: ', data);
    console.log('dddd');
    
    return <div>Test</div>
}