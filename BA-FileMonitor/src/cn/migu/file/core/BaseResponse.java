package cn.migu.file.core;

/**
 * 与unify_m操作响应类
 * 
 * @author  zhaocan
 * @version  [版本号, 2016年3月25日]
 * @see  [相关类/方法]
 * @since  [产品/模块版本]1
 */
public class BaseResponse
{
    private BaseResponseEntity response;
    
    public BaseResponse()
    {
    }
    
    public BaseResponse(BaseResponseEntity response)
    {
        this.response = response;
    }
    
    public BaseResponseEntity getResponse()
    {
        return response;
    }
    
    public void setResponse(BaseResponseEntity response)
    {
        this.response = response;
    }
    
}
