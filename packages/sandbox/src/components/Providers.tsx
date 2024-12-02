"use client";

import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { TabContext } from "./EditorsView/Tabs";

const queryClient = new QueryClient();
export const Providers = ({ children }: { children: React.ReactNode }) => {
	return (
		<QueryClientProvider client={queryClient}>
			<TabContext> {children}</TabContext>
		</QueryClientProvider>
	);
};
