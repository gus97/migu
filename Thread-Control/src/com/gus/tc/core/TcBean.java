package com.gus.tc.core;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.FutureTask;

/**
 * 
 * @author guxichang
 *
 */

public class TcBean {

	private ExecutorService executorService;
	private List<Thread> threads = new ArrayList<Thread>();
	private FutureTask<Object> futureTask;
	
	/**
	 * 如果是中断模式，必须遵循中断结构
	 */
	private Integer finishTag;
	private Long distributedIDS;

	//当前主动开启的所有线程（开发自己能控制的线程）
	public List<Thread> getThreads() {
		return threads;
	}

	public ExecutorService getExecutorService() {
		return executorService;
	}

	public void setExecutorService(ExecutorService executorService) {
		this.executorService = executorService;
	}

	public void setThreads(List<Thread> threads) {
		this.threads = threads;
	}

	public FutureTask<Object> getFutureTask() {
		return futureTask;
	}

	public void setFutureTask(FutureTask<Object> futureTask) {
		this.futureTask = futureTask;
	}

	public Integer getFinishTag() {
		return finishTag;
	}

	public void setFinishTag(Integer finishTag) {
		this.finishTag = finishTag;
	}

	public Long getDistributedIDS() {
		return distributedIDS;
	}

	public void setDistributedIDS(Long distributedIDS) {
		this.distributedIDS = distributedIDS;
	}

}
