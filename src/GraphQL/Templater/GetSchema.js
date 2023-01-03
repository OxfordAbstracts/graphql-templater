import "isomorphic-fetch";
import { getIntrospectionQuery, printSchema, buildClientSchema } from "graphql";

export function getGqlSchemaImpl({ url, token, headers }) {
  return async () => {
    try {
      const introspectionQuery = getIntrospectionQuery();

      const defaultHeaders = token
      ? {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        }
      : {
          "Content-Type": "application/json",
        }

      const response = await fetch(url, {
        method: "POST",
        headers: {...defaultHeaders, ...headers},
        body: JSON.stringify({ query: introspectionQuery }),
        credentials: "include",
      });

      const { data } = await response.json();

      const schema = printSchema(buildClientSchema(data));

      return schema;
    } catch (err) {
      console.error("failed to get gql schema", err);
      throw err;
    }
  };
}
