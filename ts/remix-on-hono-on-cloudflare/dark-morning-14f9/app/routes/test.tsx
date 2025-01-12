import { json, useLoaderData } from "@remix-run/react"

export const loader = () => {
    return json({
        test: 'hello',
    })
}

export default function Test() {
    const data = useLoaderData()
    console.log('data: ', data);
    
    return <div>Test</div>
}