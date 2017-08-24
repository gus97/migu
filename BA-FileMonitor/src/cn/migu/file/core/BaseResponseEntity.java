package cn.migu.file.core;

/**
 * 与unify_m交互响应内容
 * 
 * @author  zhaocan
 * @version  [版本号, 2016年3月25日]
 * @see  [相关类/方法]
 * @since  [产品/模块版本]
 */
public class BaseResponseEntity
{
    //成功为00,其他为01
    private String code;
    
    //具体描述信息
    private String desc;
    
    //具体内容信息
    private String content;
    
    public String getCode()
    {
        return code;
    }
    
    public void setCode(String code)
    {
        this.code = code;
    }
    
    public String getDesc()
    {
        return desc;
    }
    
    public void setDesc(String desc)
    {
        this.desc = desc;
    }
    
    public String getContent()
    {
        return content;
    }
    
    public void setContent(String content)
    {
        this.content = content;
    }
    
}
