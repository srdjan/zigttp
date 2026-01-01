// TypeScript + JSX handler example

type Props = {
    name: string;
    count: number;
};

interface GreetingProps {
    message: string;
}

function Greeting(props: GreetingProps): JSX.Element {
    return <h1>{props.message}</h1>;
}

function Card(props: Props): JSX.Element {
    return (
        <div class="card">
            <Greeting message={"Hello, " + props.name} />
            <p>Count: {props.count}</p>
        </div>
    );
}

function handler(req: Request): Response {
    const data: Props = { name: "World", count: 42 };
    const html: string = renderToString(<Card name={data.name} count={data.count} />);
    return Response.html(html);
}
