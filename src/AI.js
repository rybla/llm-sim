import { CohereClient } from "cohere-ai";

export const make_client = ({ token }) => () => {
  return new CohereClient({ token })
}

export const chat_ = ({ client, request }) => async () => {
  return await client.chat(request)
}
