// Shared utility functions for handler examples

export function greet(name: string): string {
  return "Hello, " + name + "!";
}

export function formatJson(data: object): string {
  return JSON.stringify(data);
}
