import { CohereClient } from "cohere-ai";

export const make_client = ({ token }) => () => {
  return new CohereClient({ token })
}

