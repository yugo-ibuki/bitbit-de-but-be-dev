import type { LoaderFunctionArgs } from "@remix-run/cloudflare"
import { json, useLoaderData } from "@remix-run/react"

export const loader = ({ context: { env } }: LoaderFunctionArgs) => {
    const key = env.API_KEY
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