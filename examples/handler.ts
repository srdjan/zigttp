// TypeScript handler example

type RequestData = {
    name: string;
    count: number;
};

interface ResponseData {
    message: string;
    timestamp: number;
}

function processData(data: RequestData): ResponseData {
    return {
        message: "Hello, " + data.name,
        timestamp: Date.now()
    };
}

function handler(req: Request): Response {
    const data: RequestData = { name: "World", count: 42 };
    const result: ResponseData = processData(data);
    return Response.json(result);
}
